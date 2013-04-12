;; jni-method-selection.scm : utilities functions to mock java overloading rules for methods
;; 
;; -this functions are used by jlambda-methods
;; 
;; [Hugo] I found two edge cases that I don't think can be easily resolved:
;; 
;; 1) Let's suppose two overloaded methods with the following signatures:
;; 
;; int bar(short)
;; int bar(int)
;; 
;; My first attempt to build the selection, was to choose the more accurate
;; type, so for example if I invoke bar as:
;; 
;; bar(1)
;; 
;; then bar(short) should be called. But, if I try the same example into java code
;; bar(int) is invoked instead. This happens simply because java haven't a short
;; (or byte) literal, so by default a number is an integer. 
;; 
;; There are two alternatives for invoking the short variant:
;; 
;; bar((short) 1) // casting
;; 
;; short n = 1;
;; bar(n);        // use a short variable
;; 
;; With this information I'm not sure what is the best option: if try to mimic
;; java behavior, and start looking from int or if simple look after the most
;; specific type. Let's move to item (2) first.
;; 
;; 2) Now suppose another schema:
;; 
;; class N1 {};
;; class N2 extends N1 {};
;; 
;; int foo(N1);
;; int foo(N2);
;; 
;; Here we can use the most specific type: if we invoke foo with N1, N1 variant will
;; be invoked, and the same for N2. However, as N2 is a subclass of N1, should be 
;; possible to invoke N1 variant with N2 (similar to (1)). The java alternatives are
;; the same than before: casting or a N1 variable.
;; 
;; For this two reason I think we can mimic java overloading rules and then
;; have a type specifier for specific invocations (of course, this could be
;; resolved simply defining a jlambda-method for the particular conflicting
;; variants, but I think is very ugly from the library user perspective).
;; 
;; Having all this in mind, this implementation works as follow:
;; 
;; jlambda-methods keep a list of available methods and signatures. When is
;; invoked look up for the best signature match, having this rules:
;; 
;; - if numeric => 
;;                fixnum     => choose the first fit in this order: (int long
;;                              float double java.lang.Integer java.lang.Long java.lang.Float
;;                              java.lang.Double)
;;                not fixnum => choose the first fit in this order: (float double java.lang.Float java.lang.Double) 
;; - if boolean => boolean
;; - if jobject => choose the most close (close in the inheritance sense) class available.
;; 
;; For the edge cases described above a new syntax is added:
;;
;; so we can invoke the previous cases this way:
;; 
;; (foo ('N1 n2))    ; an argument could be a list of the form (TYPE arg)
;; (bar ('short 1))

(define prefered-fixnum-types '(int long float double java.lang.Integer java.lang.Long java.lang.Float java.lang.Double))
(define prefered-flonum-types '(float double java.lang.Float java.lang.Double))

;; generate a list of the form ((parameter-signature . jlambda-method) ...)
(define (generate-methods modifiers class-name method-name signatures)
	(fold (lambda (signature methods)
					(let ((return-type (car signature))
								(args-type   (cdr signature)))
						(cons (cons args-type (jlambda-method-imple modifiers return-type class-name method-name args-type)) methods)))
				'()
				signatures))

;; check if the args match the type signature
(define (match-arg-types args types)
  (and (= (length args) (length types))
       (every (lambda (arg type)
                (if (pair? arg)
                  (eq? (car arg) type)
                  (type-case arg
                             (boolean (eq? 'boolean type))
                             (number  
                               (if (fixnum? arg)
                                 ;TODO: improve this to avoid callling jlambda-field each time
                                 (or (and (member type '(java.lang.Integer int))
                                          (< arg (expt 2 31))
                                          (>= arg (- (expt 2 31))))
                                     (and (member type '(java.lang.Long long))
                                          (< arg (expt 2 63))
                                          (>= arg (- (expt 2 63))))
                                     (and (member type '(java.lang.Float float))
                                          (< arg ((jlambda-field (static) float java.lang.Float MAX_VALUE))) 
                                          (> arg ((jlambda-field (static) float java.lang.Float MIN_VALUE))))
                                     (member type '(java.lang.Double double)))
                                 (or (and (member type '(java.lang.Float float))
                                          (fp<= arg ((jlambda-field (static) float java.lang.Float MAX_VALUE))) 
                                          (fp>= arg ((jlambda-field (static) float java.lang.Float MIN_VALUE))))
                                     (member type '(java.lang.Double double)))))
                             (string
                               (and-let* ((type-class (find-class (mangle-class-name type))))
                                 (assignable-from? (find-class "java/lang/String") type-class)))
                             (jobject
                               (if (not (primitive? type))
                                 (let ((type-class (find-class (mangle-class-name type))))
                                   (and type-class
                                        (instance-of? arg type-class)))
                                 #f))
                             (char (eq? type 'char))
                             (else (assert #f)))))
              args types)))

(define (integer-compare n1 n2)
  (cond ((= n1 n2)   0)
        ((< n1 n2)  -1)
        (#t          1)))

;; compare two types returns -1 if type1 is better than type2, 0 if they are
;; equals, and 1 if type2 is better than type1
(define (type-compare type1 type2)
  (cond ((eq? type1 type2)
         0)
        ((and (member type1 prefered-fixnum-types)
              (member type2 prefered-fixnum-types))
         (integer-compare (list-index (cut eq? <> type1) prefered-fixnum-types)
                          (list-index (cut eq? <> type2) prefered-fixnum-types)))
        ((and (member type1 prefered-flonum-types)
              (member type2 prefered-flonum-types))
         (integer-compare (list-index (cut eq? <> type1) prefered-flonum-types)
                          (list-index (cut eq? <> type2) prefered-flonum-types)))
        (#t
         (let ((type1-class (find-class (mangle-class-name type1)))
               (type2-class (find-class (mangle-class-name type2))))
           (assert (and type1-class type2-class))
           (if (assignable-from? type1-class type2-class) -1 1)))))

(define (best-method m1 m2)
  (let loop ((args-1 (car m1))
             (args-2 (car m2))
             (w1 0)
             (w2 0))
    (if (null? args-1)
      (cond ((= w1 w2) (error 'jlambda-method "Ambiguos method information: " m1 m2))
            ((> w1 w2) m1)
            (else      m2))
      (let* ((r  (type-compare (car args-1) (car args-2)))
             (w1 (if (< r 0) (+ w1 1) w1))
             (w2 (if (> r 0) (+ w2 1) w2)))
        (loop (cdr args-1) (cdr args-2) w1 w2)))))

(define (find-method-match methods args)
  (fold (lambda (method best)
          (if (match-arg-types args (car method))
            (if best 
              (best-method method best) 
              method)
            best))
        #f
        methods))
