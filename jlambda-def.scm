(use jni lolevel expand-full moremacros jni-reflection)

(define android-sdk-path "/home/paul/opt/adt-bundle-linux-x86/sdk")
(define android-platform-version 14)
(define android-platform-path 
  (string-append android-sdk-path "platforms/android-" (number->string android-platform-version) "/"))
(define android-platform-jar-path
  (string-append android-platform-path "android.jar"))

;;(jvm-init android-platform-jar-path)
;(jvm-init)

(import-for-syntax jni)
(define (exc wut)
  (if (exception-check)
    (begin
      (printf "~s~n" wut)
      (exception-describe)
      (exception-clear))))

(define-for-syntax (jvm-init-macros)
                   (jvm-init))
(jvm-init-macros)

(define-syntax %
  (syntax-rules ()
                ((_ arg body)
                 (lambda (arg)
                   body))))
(define id (% v v))

(define primitive-types-score '(("byte" 0) ("short" 10) ("int" 20) ("long" 30) ("float" 40) ("double" 50)))

(define (is-num? guard)
  (lambda (v)
    (and (number? v) (guard v))))

(define (make-decl-type class)
  (list (to-string class) class))

(define (decl-type-type decl-type)
  (cadr decl-type))
(define (decl-type-primitive? decl-type)
  (assoc (car decl-type) primitive-types-score))

(define (prim-type-matcher names)
  (lambda (decl-type)
    (let ((decl-class (decl-type-type decl-type)))
      (if (call decl-class 'isPrimitive)
        (let ((simple-name (to-string (call decl-class 'getSimpleName))))
          (printf "check simple-name: ~s in ~n" simple-name names)
          (assoc simple-name names))
        (begin
          (printf "Not primitive: ~s~n" (to-string decl-class))
          #f
          )))))

(define (make-type type-name value-matcher jvalue-type matcher)
  (list type-name value-matcher jvalue-type matcher))

(define (make-prim-type* type-name value-matcher jvalue-type prim-type-names)
  (make-type type-name value-matcher jvalue-type (prim-type-matcher prim-type-names)))
(define (type->matcher type)
  (cadr type))
(define (type->type-matcher type)
  (cadddr type))
(define (type->jvalue type value)
  ((caddr type) value))

(define primitive-types
  (list
    (make-prim-type* "numeric" number? (% v (cond 
                                              ((and (<= v 127) (>= v -128)) 'byte)
                                              ((and (<= v 32767) (>= v -32768)) 'short)
                                              ((and (<= v 2147483647) (>= v -2147483648)) 'int) ;; whut?
                                              ((not (fixnum? v)) 'float)))
                                                    
                                                    
                                                    primitive-types-score)
    (make-type "string" string?
               (% v 'java.lang.String)
                    (let ((jlstring (find-class "java/lang/String")))
                      (lambda (decl-type)
                        (call (decl-type-type decl-type) 'equals jlstring))))
    ))

(define (prim-value->type value)
  (if (not (pointer? value))
    (find (% type ((type->matcher type) value))
            primitive-types)))
(define (pointer-value->type value)
  (if (pointer? value)
    (make-type (to-string value)
               #f
               (% v 'java.lang.Object)
               (lambda (decl-type)
                 (call (decl-type-type decl-type) 'isInstance value)))))

(define (value->type value)
  (if (pointer? value)
    (pointer-value->type value)
    (prim-value->type value)))
(define (values->types . valuez)
  (map value->type valuez))
(define (values->jvalues . valuez)
  (map type->jvalue
       (map value->type valuez)
       valuez))

(define (values->jvalues-array . valuez)
  (printf "values->jvalues-array: ~s~n" valuez)
  (if (null? valuez)
    #f
    (let ((types (apply values->jvalues valuez))
          (jvalue-array (make-jvalue-array (length valuez))))
      (fold (lambda (arg i)
              (let ((type (list-ref types i)))
                (printf "Setting ~s to ~s~n" i arg)
                (if (pointer? arg)
                  (set-object-jvalue! jvalue-array i arg)
                  (case type
                    ((boolean)
                     (set-boolean-jvalue! jvalue-array i arg))
                    ((char)
                     (set-char-jvalue! jvalue-array i (integer->char arg)))
                    ((byte)
                     (set-byte-jvalue! jvalue-array i (integer->char arg)))
                    ((short)
                     (set-short-jvalue! jvalue-array i arg))
                    ((int)
                     (set-int-jvalue! jvalue-array i arg))
                    ((long)
                     (set-long-jvalue! jvalue-array i arg))
                    ((float)
                     (set-float-jvalue! jvalue-array i arg))
                    ((double)
                     (set-double-jvalue! jvalue-array i arg))
                    (else
                      (error (format "Can't match jvalue argument ~s" type) "baaaaaaar!!!!")))))
              (+ i 1))
            0
            valuez)
      jvalue-array)))

(define (match-type-to-decl-type type decl-type)
  (printf "Matching type ~s to ~s ~n" type decl-type)
  (let ((matcher (type->type-matcher type)))
    (printf "matched: ~s~n" (matcher decl-type))
    (matcher decl-type)))

;; match single method declaration with arg-types
(define (match-method-types arg-types method-types)
  (let ((matched (map (lambda (arg-type decl-type)
                 (if (match-type-to-decl-type arg-type decl-type)
                   decl-type
                   #f
                   ))
               arg-types method-types)))
    (printf "match-method-types: ~s to ~s is ~s~n" arg-types method-types matched)
    (if (member #f matched)
      #f
      matched)))

;; match all method declarations with arg-types
(define (match-methods-types arg-types methods-types)
  (filter id (map (% method-types (match-method-types arg-types method-types)) methods-types)))

(define (is-superclass? class-super class-child)
  (let loop ((super-classes (get-class-hierarchy class-child)))
    (if (null? super-classes)
      #f
      (let ((super-class (car super-classes)))
        (if (call super-class 'equals class-super)
          #t
          (loop (cdr super-classes)))))))

;; determines if decl-type1 is better then decl-type2
;; the type1 is better of type2 if type2 is super-class of type1, so it is more precise
(define (compare-decl-type decl-type1 decl-type2)
  (printf "compare ~s to ~s~n" decl-type1 decl-type2)
  ;; let's check is this prim type
  (let ((prim1 (decl-type-primitive? decl-type1))
        (prim2 (decl-type-primitive? decl-type2)))
    (if (and prim1 prim2)
      (> (cadr prim1) (cadr prim2))
      (let ((decl-type1-class (decl-type-type decl-type1))
            (decl-type2-class (decl-type-type decl-type2)))
        (is-superclass? decl-type2-class decl-type1-class)))))

;; match1 is better match2 when at least one type in match1 is subclass of match2
(define (compare-matches match1 match2)
  (if (or (not match2) (null? match2))
    #t
    (let loop ((match1 match1)
               (match2 match2))
      (if (or (null? match1) (not match1))
        #f
        (let ((decl-type1 (car match1))
              (decl-type2 (car match2)))
          (if (compare-decl-type decl-type1 decl-type2)
            #t
            (loop (cdr match1) (cdr match2))))))))


(define (match-method arg-types methods)
  (if (and (null? arg-types)
           (not (null? methods))
           (null? (cdr methods)))
    (car methods)
    (let loop ((methods (get-decl-method-types (get-method-types methods)))
               (best-match '()))
      (if (null? methods)
        best-match
        (let* ((method-desc (car methods))
               (foo (printf "method-desc: ~s~n" method-desc))
               (decl-types (cadr method-desc))
               (foo (printf "decl ~s~n" decl-types))
               (matched (match-method-types arg-types decl-types))
               )
          (printf "matched? ~s~n" matched)
          (if (or (null? best-match)
                  (compare-matches matched (cdr best-match)))
            (loop (cdr methods) (cons (car method-desc) matched))
            (loop (cdr methods) best-match)))))))

;; hlist: ((key value1 value2 ...) (key value3 value4 ...) ...)
(define (hlist-put key value hlist cmp)
  (let ((current (assoc key hlist cmp))
        )
    (if current
      ;; there is (key value ...)
      (let* ((hlist* (alist-delete key hlist cmp)) ;; remove old list from hlist
             )
        (append hlist* (list (cons key (append (cdr current) (list value)))))) ;; build new list (key value @current)
      ; there is no (key value), simply append new list to hlist
      (append hlist (list (cons key (list value)))))))

;; (list-build car eq? '((a 1) (a 2) (b 1) (c 2) (c 4))) 
;; ->
;; ((a ((a 1) (a 2))) (b ((b 1))) (c ((c 2) (c 4))))
(define (list-build pred cmp lizt)
  (let loop ((lizt lizt)
             (res '()))
    (if (null? lizt)
      res
      (let* ((elt (car lizt))
             (elt-name (pred elt))
             (elt-res (assoc elt-name res cmp))
             )
        (loop (cdr lizt)
              (hlist-put elt-name elt res cmp))))))

(define-for-syntax (class-methods class)
                    (methods class))
(define-for-syntax (class-by-name class-name)
                   (find-class class-name))

(define-for-syntax (get-method-list methods)
    (let ((by-names (list-build (lambda (m) (to-string (method-name m)))
                             string=?
                             methods)))
    (map (lambda (ms)
           (let ((name (car ms))
                 (overloaded (cdr ms)))
             (cons name
                    (list-build (lambda (method) (length (method-parameters method)))
                                =
                                overloaded))))
      by-names)))



(define class-name "java/lang/String")
(define class (find-class "java/lang/Object"))
(define get-class/m (get-method-id class "getClass" "()Ljava/lang/Class;"))
(define (get-class object) (call-object-method object get-class/m #f))
(define is-java/lang/object?
  (let ((j/l/o (find-class "java/lang/Object")))
    (lambda (class)
      (call j/l/o 'equals class))))

(define (is-top-hier class)
  (or (not class)
       (is-java/lang/object? class)))

(define (get-super-classes class)
  ;(printf "Get superclass of ~s~n" (to-string class))
  (let loop ((super-classes '())
             (class class))
    (if (is-top-hier class)
      (begin
   ;     (printf "SC: ~s~n" super-classes)
        super-classes)
      (let ((super-class (call class 'getSuperclass)))
        (if super-class
          (loop (cons super-class super-classes) super-class)
          super-classes)))))

(define (get-class-hierarchy class)
  (let* ((clazz (get-object-class class))
         (interfaces-m (get-method-id clazz "getInterfaces" "()[Ljava/lang/Class;"))
         (interfaces (array->list (call-object-method class interfaces-m #f)))
         (super-classes (get-super-classes class)))
    ;(printf "Interfaces: ~s~n" interfaces)
    (let loop ((hier super-classes)
               (interfaces interfaces))
      (if (null? interfaces)
        hier
        (let* ((interface (car interfaces))
               (super-interfaces (get-super-classes interface))
               )
          (printf "SI: ~s~n" super-interfaces)
          (loop (append hier (if super-interfaces
                               (append (list interface) super-interfaces)
                               (list interface)))
                (cdr interfaces)))))))


(define-for-syntax (get-ver)
                   (version))

(define (get-declared-types method)
  (method-parameters method))
(define (get-method-types methods)
  (map (% method (list method (get-declared-types method))) methods))
(define (get-decl-method-types methods-desc)
  (map (% method-desc (list (car method-desc) (map make-decl-type (cadr method-desc)))) methods-desc))
(define (methods-by-name/arn object method-name arn)
  (let ((methods (methods-by-name object method-name)))
    (filter (% method (let ((method-arn (length (method-parameters method))))
                        (= method-arn arn)))
            methods)))

(define (make-overloaded-method-caller name)
  (lambda (object . args)
    (printf "o/c: ~s, args: ~s~n" object args)
    (let* ((arn (length args))
           (methodz (methods (get-class object)))
           (method-list (get-method-list methodz))
           (named-methods (assoc name method-list))
           (foo1 (printf "named: ~s~n" named-methods))
           )

      (if (not named-methods)
        (error (format "Can't find method ~s ~n" name))
        (let ((a-methods (assoc arn (cdr named-methods))))
          (if (not a-methods)
            (error (format "Can' find method ~s with arn ~s~n" name arn))
            (let* ((o-methods (cdr a-methods))
                   (method (match-method args o-methods)))
              (printf "Method: ~s~n" method)
              (if method
                (begin
                  (printf "found method: ~s.~s(~s)~n" (to-string object) (to-string method) (apply values->jvalues-array args))
                (call-boolean-method object (Method->method-id method) (apply values->jvalues-array args)))
                (error (format "Can't match ~s with method ~s~n" args name))))))))))

;; (jlambda java.lang.String String) declares
;; (String-valueOf object arg)
;; (String-length) and so on
(define-syntax  jlambda
  (er-macro-transformer
    (lambda (x r c)
      (printf "~s~n" (cadr x))
      (let* ((%lambda (r 'lambda))
             (%define (r 'define))
             (%begin (r 'begin))
             (%make-overloaded-method-caller (r 'make-overloaded-method-caller))

             (class-name (mangle-class-name(cadr x)))
             (local-name (symbol->string (caddr x)))
             (define-name local-name)
             (methods (class-methods (class-by-name class-name)))
             (method-list (get-method-list methods))
             )
        (printf "method-list: ~s~n" method-list)
        (cons
          %begin
          (map (lambda (method-desc)
                 (let* ((method-name (car method-desc))
                        (define-name (string->symbol (string-append local-name "-" method-name)))
                        )
                   (printf "defining ~s, ~s~n" define-name (cdr method-desc))
                   `(,%define ,define-name (make-overloaded-method-caller ,method-name))))
               method-list))))))

(define-syntax jlambda-method
  (er-macro-transformer
    (lambda (x r c)
      (let ((%lambda (r 'lambda))
            (%let (r 'let*))
            (%find-class (r 'find-class))
            (%get-method-id (r 'get-method-id))
            (%get-static-method-id (r 'get-static-method-id))
            (%make-jvalue-array (r 'make-jvalue-array))
            (%free-jvalue-array (r 'free-jvalue-array))
            (%delete-local-ref (r 'delete-local-ref))
            (%delete-global-ref (r 'delete-global-ref))
            (%if (r 'if))
            (%exception-check (r 'exception-check))
            (%exception-describe (r 'exception-describe))
            (%error (r 'error))
            (%begin (r 'begin))
            (%method-id->Method (r 'method-id->Method))
            (%format (r 'format))

            (class-type (cadr x))
            (return-type (caddr x))
            (method-name (car (cdddr x))))
        (let* ((argument-types  (cddddr x))
               (argument-names (map (lambda (arg-count)
                                      (string->symbol (format "a~A" arg-count)))
                                    (iota (length argument-types) 1 1))))

          `(extend-procedure
             (,%let ((class-object (,%find-class ,(mangle-class-name class-type)))
                     (method-id (,%get-method-id class-object ,(symbol->string method-name) (type-signature ,argument-types ,return-type)))
                     (static-method-id (,%get-static-method-id class-object ,(symbol->string method-name) (type-signature ,argument-types ,return-type)))
                     (is-static? (and static-method-id #t))
                     (jmethod (,%if is-static? static-method-id method-id)))
                    (exception-clear) ;; clear NoSuchMethodFound
                    (,%if is-static?
                          (,%lambda (,@argument-names)
                                    (,%let ((jvalues ,(if (null? argument-types) #f `(jvalue-zip ,argument-types ,@argument-names)))
                                            (return-value (call-method #t class-object ,return-type jmethod jvalues)))
                                           (,%if (,%exception-check)
                                                 (,%begin 
                                                   (,%exception-describe)
                                                   (,%error (append '(static ,class-type ,method-name) (list return-value jmethod ,@argument-names))))
                                                 return-value)))
                          (,%lambda (,@(append '(object) argument-names))
                                    (,%let ((jvalues ,(if (null? argument-types) #f `(jvalue-zip ,argument-types ,@argument-names)))
                                            (return-value (call-method #f object ,return-type jmethod jvalues)))
                                           (,%if (,%exception-check)
                                                 (,%begin 
                                                   (,%exception-describe)
                                                   (,%error (append '(,class-type ,method-name) ,@argument-names)))
                                                 return-value)))))
             ',argument-types))))))
(jlambda java.lang.String String)
(define o-class (find-class "o"))
(define o2-class (find-class "o$o2"))
(define mtests (methods-by-name/arn o-class 'mtest 2))
(define m1 (car mtests))
(define m1t (method-parameters m1))
(define m-types (get-method-types mtests))
;(define m-type (car m-types))
(define o-obj (call o-class 'newInstance))
(define args (list "abc" o-obj))
(define arg-types (map value->type args))
;(define m-type1 (make-decl-type (car m-type)))
;(define decl-types  (map make-decl-type m-type))
(define a1 (jstring "abc"))
(define a-type1 (pointer-value->type a1))
;(define method-decl-types (map (% v (map make-decl-type v)) m-types))
(define int (cadr (list-ref (map cadr (get-decl-method-types m-types )) 4)))
(define i1 (value->type 1))
(define mtest/1 (methods-by-name/arn o-class 'mtest 1))
(define mtypes/1 (get-method-types mtest/1))
