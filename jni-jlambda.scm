; symbol => jclass
(define-syntax class
  (ir-macro-transformer
   (lambda (x i c)
     (let ((name (mangle-class-name (strip-syntax (cadr x)))))
       `(find-class ,name)))))

(define-syntax method*
  (syntax-rules ()
    ((_ fn return class-name name args ...)
     (let* ((class-object (class class-name))
            (return-value (fn class-object
                              (symbol->string 'name)
                              (type-signature (args ...) return))))
       (delete-local-ref class-object)
       return-value))))

(define-syntax method
  (syntax-rules ()
    ((_ args ...)
     (method* get-method-id args ...))))

(define-syntax static-method
  (syntax-rules ()
    ((_ args ...)
     (method* get-static-method-id args ...))))

(define-syntax constructor
  (er-macro-transformer
   (lambda (x r c)
     `(,(r 'method) void ,(cadr x) <init> . ,(cddr x)))))

(define-syntax define-method
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((name (mangle-method-name (strip-syntax (caadr x)))))
       `(define-external (,(i name)
                          (,(i '(c-pointer "JNIEnv")) env)
                          (,(i '(c-pointer "jobject")) ,(cadadr x))
                          . ,(cddadr x))
          ,(i (caddr x))
          (parameterize ((jni-env env))
            . ,(cdddr x)))))))

;; convenient procedure to delegate invocation for the apropiate call (by modifier and return type)
(define-syntax call-method
  (er-macro-transformer
    (lambda (x r c)
      (let ((%call-static-void-method (r 'call-static-void-method))
            (%call-static-boolean-method (r 'call-static-boolean-method))
            (%call-static-object-method (r 'call-static-object-method))
            (%call-static-byte-method (r 'call-static-byte-method))
            (%call-static-char-method (r 'call-static-char-method))
            (%call-static-short-method (r 'call-static-short-method))
            (%call-static-int-method (r 'call-static-int-method))
            (%call-static-long-method (r 'call-static-long-method))
            (%call-static-float-method (r 'call-static-float-method))
            (%call-static-double-method (r 'call-static-double-method))
            (%call-void-method (r 'call-void-method))
            (%call-boolean-method (r 'call-boolean-method))
            (%call-object-method (r 'call-object-method))
            (%call-byte-method (r 'call-byte-method))
            (%call-char-method (r 'call-char-method))
            (%call-short-method (r 'call-short-method))
            (%call-int-method (r 'call-int-method))
            (%call-long-method (r 'call-long-method))
            (%call-float-method (r 'call-float-method))
            (%call-double-method (r 'call-double-method))
            (%prepare-local-jobject (r 'prepare-local-jobject))

            (modifiers (cadr x))
            (object  (caddr x))
            (return-type (cadddr x))
            (jmethod (car (cddddr x)))
            (jvalues (cadr (cddddr x))))

        (if modifiers
          (case return-type
            ((void)   `(,%call-static-void-method    ,object ,jmethod ,jvalues))
            ((boolean)`(,%call-static-boolean-method ,object ,jmethod ,jvalues))
            ((byte)   `(,%call-static-byte-method    ,object ,jmethod ,jvalues))
            ((char)   `(,%call-static-char-method    ,object ,jmethod ,jvalues))
            ((short)  `(,%call-static-short-method   ,object ,jmethod ,jvalues))
            ((int)    `(,%call-static-int-method     ,object ,jmethod ,jvalues))
            ((long)   `(,%call-static-long-method    ,object ,jmethod ,jvalues))
            ((float)  `(,%call-static-float-method   ,object ,jmethod ,jvalues))
            ((double) `(,%call-static-double-method  ,object ,jmethod ,jvalues))
            (else     `(,%prepare-local-jobject (,%call-static-object-method  ,object ,jmethod ,jvalues))))
          (case return-type
            ((void)   `(,%call-void-method    ,object ,jmethod ,jvalues))
            ((boolean)`(,%call-boolean-method ,object ,jmethod ,jvalues))
            ((byte)   `(,%call-byte-method    ,object ,jmethod ,jvalues))
            ((char)   `(,%call-char-method    ,object ,jmethod ,jvalues))
            ((short)  `(,%call-short-method   ,object ,jmethod ,jvalues))
            ((int)    `(,%call-int-method     ,object ,jmethod ,jvalues))
            ((long)   `(,%call-long-method    ,object ,jmethod ,jvalues))
            ((float)  `(,%call-float-method   ,object ,jmethod ,jvalues))
            ((double) `(,%call-double-method  ,object ,jmethod ,jvalues))
            (else     `(,%prepare-local-jobject (,%call-object-method  ,object ,jmethod ,jvalues)))))))))

(define-syntax jvalue-zip
  (er-macro-transformer
    (lambda (x r c)
      (let ((%let                 (r 'let))
            (%make-jvalue-array   (r 'make-jvalue-array))
            (%set-boolean-jvalue! (r 'set-boolean-jvalue!))
            (%set-byte-jvalue!    (r 'set-byte-jvalue!))
            (%set-char-jvalue!    (r 'set-char-jvalue!))
            (%set-int-jvalue!     (r 'set-int-jvalue!))
            (%set-long-jvalue!    (r 'set-long-jvalue!))
            (%set-float-jvalue!   (r 'set-float-jvalue!))
            (%set-double-jvalue!  (r 'set-double-jvalue!))
            (%set-object-jvalue!  (r 'set-object-jvalue!))
            (%if                  (r 'if))
            (%car                 (r 'car))
            (%string?             (r 'string?))
            (%jstring             (r 'jstring))
            (argument-types       (cadr x))
            (argument-names       (cddr x)))

        `(,%let ((jvalues (,%make-jvalue-array ,(length argument-types))))
                ,@(map (lambda (argument-type arg index)
                         (case argument-type
                           ((boolean) `(,%set-boolean-jvalue! jvalues ,index ,arg))
                           ((byte)    `(,%set-byte-jvalue!    jvalues ,index ,arg))
                           ((char)    `(,%set-char-jvalue!    jvalues ,index ,arg))
                           ((short)   `(,%set-short-jvalue!   jvalues ,index ,arg))
                           ((int)     `(,%set-int-jvalue!     jvalues ,index ,arg))
                           ((long)    `(,%set-long-jvalue!    jvalues ,index ,arg))
                           ((float)   `(,%set-float-jvalue!   jvalues ,index ,arg))
                           ((double)  `(,%set-double-jvalue!  jvalues ,index ,arg))
                           ((java.lang.String java.lang.CharSequence java.lang.Object)
                            `(,%if (,%string? ,arg)
                                   (,%set-object-jvalue! jvalues ,index (,%jstring ,arg)) ;; wont be GCed :(
                                   (,%set-object-jvalue! jvalues ,index ,arg)))
                           (else
                             `(,%set-object-jvalue! jvalues ,index ,arg))))
                       argument-types
                       argument-names
                       (iota (length argument-types))))))))

;; (jlambda-method [modifiers] CLASS RETURN-TYPE METHOD-NAME ARGS...) => lambda
(define-syntax jlambda-method
  (er-macro-transformer
    (lambda (x r c)
      (let ((%lambda               (r 'lambda))
            (%let                  (r 'let*))
            (%class                (r 'class))
            (%method               (r 'method))
            (%static-method        (r 'static-method))
            (%make-jvalue-array    (r 'make-jvalue-array))
            (%free-jvalue-array    (r 'free-jvalue-array))
            (%if                   (r 'if))
            (%exception-check      (r 'exception-check))
            (%error                (r 'error))
            (%format               (r 'format))
            (modifiers             (cadr x))
            (return-type           (caddr x))
            (class-type            (cadddr x))
            (method-name           (car (cddddr x)))
            (argument-types        (cdr (cddddr x))))
        (let* ((argument-names  (map (lambda (arg-count)
                                       (string->symbol (format "a~A" arg-count)))
                                     (iota (length argument-types) 1 1))))
          `(extend-procedure
             (,%lambda (,@(append (if modifiers '() '(object)) argument-names))
                       (,%let ((class-object (,%class ,class-type))
                               (jmethod      (,(if modifiers %static-method %method) 
                                               ,return-type ,class-type ,method-name ,@argument-types))
                               (jvalues      ,(if (null? argument-types) #f `(jvalue-zip ,argument-types ,@argument-names)))
                               (return-value (,%if jmethod 
                                                   (call-method ,modifiers ,(if modifiers 'class-object 'object) 
                                                                ,return-type jmethod jvalues)
                                                   (,%error 'jlambda-method "method not found"))))
                              (,%free-jvalue-array jvalues)

                              (,%if (,%exception-check)
                                    (,%error 'fooooo)
                                    return-value)))
             ',argument-types))))))

(define-syntax jlambda-constructor
  (er-macro-transformer
    (lambda (x r c)
      (let ((%lambda                (r 'lambda))
            (%let                   (r 'let*))
            (%class                 (r 'class))
            (%method                (r 'method))
            (%make-jvalue-array     (r 'make-jvalue-array))
            (%free-jvalue-array     (r 'free-jvalue-array))
            (%if                    (r 'if))
            (%exception-check       (r 'exception-check))
            (%error                 (r 'error))
            (%format                (r 'format))
            (%new-object            (r 'new-object))
            (%prepare-local-jobject (r 'prepare-local-jobject))
            (class-type             (cadr x))
            (argument-types         (cddr x)))
        (let* ((argument-names  (map (lambda (arg-count)
                                       (string->symbol (format "a~A" arg-count)))
                                     (iota (length argument-types) 1 1))))
          `(extend-procedure
             (,%lambda (,@argument-names)
                       (,%let ((jmethod      (,%method void ,class-type <init> ,@argument-types))
                               (jvalues      ,(if (null? argument-types) #f `(jvalue-zip ,argument-types ,@argument-names)))
                               (return-value (,%if jmethod 
                                                   (,%prepare-local-jobject (,%new-object (,%class ,class-type) jmethod jvalues))
                                                   (,%error 'jlambda-constructor "method not found"))))
                              (,%free-jvalue-array jvalues)
                              (,%if (,%exception-check)
                                    (,%error 'fooooo)
                                    return-value)))
             ',argument-types))))))

(define-syntax field-accessor-for 
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%let*           (r 'let*))
             (%if             (r 'if))
             (%symbol->string (r 'symbol->string))
             (%string->symbol (r 'string->symbol))
             (%string-append  (r 'string-append))
             (accessor-type   (cadr x))
             (static?         (caddr x))
             (type            (cadddr x)))

        `(,%let* ((accessor-type (,%symbol->string ',accessor-type))
                  (modifier (,%if ,static? "static-" ""))
                  (type (if (primitive? ,type)
                          (,%symbol->string ',type)
                          "object"))
                  (accessor-name (,%string-append accessor-type "-" modifier type "-field")))
                 (eval (,%string->symbol accessor-name)))))))

(define-syntax static-field
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%let*               (r 'let*))
             (%class              (r 'class))
             (%if                 (r 'if))
             (%error              (r 'error))
             (%lambda             (r 'lambda))
             (%getter-with-setter (r 'getter-with-setter))
             (%type-signature     (r 'type-signature))
             (%get-field-id       (r 'get-static-field))
             (%field-accessor-for (r 'field-accessor-for))
             (modifiers           (cadr x))
             (type                (caddr x))
             (class-name          (cadddr x))
             (field-name          (symbol->string (car (cddddr x)))))
        `(,%let* ((signature (,%type-signature ,type))
                  (jclass    (,%class ,class-name))
                  (field-id  (,%get-field-id jclass ,field-name signature)))
                 (,%if field-id
                       (,%getter-with-setter (,%lambda ()
                                                       ((,%field-accessor-for get #t ,type) jclass field-id))
                                             (,%lambda (value)
                                                       ((,%field-accessor-for set #t ,type) jclass field-id value)))
                       (,%error 'static-field "field not found")))))))

(define-syntax field
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%let*               (r 'let*))
             (%class              (r 'class))
             (%if                 (r 'if))
             (%error              (r 'error))
             (%lambda             (r 'lambda))
             (%getter-with-setter (r 'getter-with-setter))
             (%type-signature     (r 'type-signature))
             (%get-field-id       (r 'get-field))
             (%field-accessor-for (r 'field-accessor-for))
             (modifiers           (cadr x))
             (type                (caddr x))
             (class-name          (cadddr x))
             (field-name          (symbol->string (car (cddddr x)))))
        `(,%let* ((signature (,%type-signature ,type))
                  (jclass    (,%class ,class-name))
                  (field-id  (,%get-field-id jclass ,field-name signature)))
                 (,%if field-id
                       (,%getter-with-setter (,%lambda (object)
                                                       ((,%field-accessor-for get #f ,type) object field-id))
                                             (,%lambda (object value)
                                                       ((,%field-accessor-for set #f ,type) object field-id value)))
                       (,%error 'field "field not found")))))))

(define-syntax jlambda-field
  (syntax-rules (static)
    ((_ (static) type class field-name)
     (static-field () type class field-name))
    ((_ (static modifier ...) type class field-name)
     (static-field (modifier ...) type class field-name))
    ((_ (modifier ... static) type class field-name)
     (static-field (modifier ...) type class field-name))
    ((_ modifiers type class field-name)
     (field modifiers type class field-name))))
