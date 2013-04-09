(use expand-full jni)
(import-for-syntax chicken)

(define (mangle-class-name name)
  (cond
   ((symbol? name)
    (case name
      ((boolean) "java/lang/Boolean")
      ((byte)    "java/lang/Byte")
      ((char)    "java/lang/Character")
      ((short)   "java/lang/Short")
      ((int)     "java/lang/Integer")
      ((long)    "java/lang/Long")
      ((float)   "java/lang/Float")
      ((double)  "java/lang/Double")
      ((void)    "java/lang/Void")
      (else (string-translate (symbol->string name) #\. #\/))))
   ((vector? name)
    (expand-type name))))

(define (expand-type type #!optional return-type)
  (cond ((symbol? type)
	 (case type
	   ((boolean) "Z")
	   ((byte)    "B")
	   ((char)    "C")
	   ((short)   "S")
	   ((int)     "I")
	   ((long)    "J")
	   ((float)   "F")
	   ((double)  "D")
	   ((void)    "V")
	   (else (string-append "L" (mangle-class-name type) ";"))))
	((vector? type)
	 (string-append "[" (expand-type (vector-ref type 0))))
	((list? type)
	 (and-let* ((return (expand-type return-type)))
	   (string-append "(" (string-intersperse (map expand-type type) "") ")" return)))))

(define-syntax type-signature
  (ir-macro-transformer
   (lambda (x i c)
     (let ((type (i (cadr x)))
	   (return-type (and (pair? (cddr x)) (i (caddr x)))))
       (expand-type type return-type)))))
(ppexpand* '(type-signature (int long long) java.lang.String))


(define-syntax class
  (ir-macro-transformer
   (lambda (x i c)
     (let ((class-name-sym (cadr x)))
       `(find-class ,(mangle-class-name (i class-name-sym)))))))
(ppexpand* '(class java.lang.String))


(define-syntax method-id
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((modifier     (cadr x))
	    (class-object (caddr x))
	    (return-type  (cadddr x))
	    (method-name  (car (cddddr x)))
	    (arg-types    (cdr (cddddr x)))
	    
	    (method-id* 
	     (or (and (c modifier 'static) 'get-static-method-id) 'get-method-id))
	    (method-name
	     (or (and (symbol? method-name) (symbol->string (i method-name))) method-name)))
       
       `(,method-id* ,class-object ,method-name (type-signature ,arg-types ,return-type))))))

(ppexpand* '(method-id static    class-object java.lang.String  valueOf  int))
(ppexpand* '(method-id static    class-object java.lang.String "valueOf" int))
(ppexpand* '(method-id nonstatic class-object java.lang.String  valueOf  int))
(ppexpand* '(method-id nonstatic class-object java.lang.String "valueOf" int))


(define-syntax set-jvalue!
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((type (cadr x))
	    (jvalue-array (caddr x))
	    (index (cadddr x))
	    (value (car (cddddr x)))
	    (set-jvalue!* (symbol-append 'set- (i type) '-jvalue!)))
       `(,set-jvalue!* ,jvalue-array ,index ,value)))))

(ppexpand* '(set-jvalue! int jvalue-array 0 a1))

(define-for-syntax (type->native-type type)
  (case type
    ((boolean byte char short int long float double) type)
    (else 'object)))
(define-for-syntax (types->native-types types)
  (map type->native-type types))

(define-for-syntax (set-jvalue! type jvalue-array index value)
  `(set-jvalue! ,type ,jvalue-array ,index ,value))

(define-syntax fill-jvalue-array
  (ir-macro-transformer
   (lambda (x i c)
     (let ((jvalue-array (cadr x))
	   (argument-types (caddr x))
	   (argument-names (cadddr x))) 
       `(begin
	  ,@(map (cut set-jvalue! <> jvalue-array <> <>)
		 (map i (types->native-types (i argument-types)))
		 (iota (length argument-types))
		 argument-names))))))

(pp (expand* '(fill-jvalue-array jvalue-array (int long long) (a1 a2 a3))))


(import-for-syntax matchable)
(define-for-syntax (call-proc-variant modifier return-type)
  (symbol-append 
   'call- 
   (case return-type ((boolean byte char short int long float double)
		      (case modifier ((static) 'static- return-type) 
			    (else return-type)))
	 (else 'object)) '-method))
(define-for-syntax (jlambda-postcall return-type call-variant)
  `(check-jexception 
    ,(case return-type
       ((boolean byte char short int long float double) call-variant)
       (else (cons 'prepare-local-jobject `(,call-variant))))))

(define-syntax jlambda-call
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier target return-type method jvalue-array)
	(let ((%variant (call-proc-variant (i modifier) (i return-type))))
	  (jlambda-postcall (i return-type) 
			    `(,%variant target ,method jvalue-array))))))))


(define-for-syntax (arg-types->arg-names* arg-types)
  (map symbol-append arg-types))
(define-for-syntax (jlambda-args modifier arg-names)
  (or (and (eq? modifier 'static) arg-names) (cons 'target arg-names)))
(define-for-syntax (class-or-free-class modifier class-object)
  (if (eq? 'static modifier)
      class-object `(delete-local-ref ,class-object)))

(define-syntax jlambda-method
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-name return-type method-name arg-types ...)
	(let ((arg-names (arg-types->arg-names* arg-types)))
	  `(let ((class-object (class ,class-name)))
	     (let ((mid (method-id ,modifier class-object ,return-type ,method-name ,@arg-types))
		   (jvalue-array (make-jvalue-array ,(length arg-types)))
		   (target ,(class-or-free-class (i modifier) 'class-object)))
	       (lambda ,(jlambda-args (i modifier) arg-names)
		 (fill-jvalue-array jvalue-array ,arg-types ,arg-names)
		 (jlambda-call ,modifier target ,return-type mid jvalue-array))))))))))

(pp (expand* '(jlambda-method static java.lang.String java.lang.String valueOf int long double)))
(pp (expand* '(jlambda-method nonstatic java.lang.String java.lang.String valueOf int long double)))
(pp (expand* '(jlambda-method nonstatic java.lang.String int valueOf int long double)))

(jvm-init)
(ppexpand* '(jlambda-method static java.lang.String java.lang.String valueOf int))


(define String.valueOf
  (jlambda-method static java.lang.String java.lang.String valueOf int))

(let loop ((i 1))
  (let ((foo (String.valueOf i)))  
    (print (format "~A> ~A" i foo) ) 
    (loop (+ 1 i))))
