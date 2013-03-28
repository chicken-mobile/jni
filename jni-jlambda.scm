(define (static-signature? modifiers)
  (if (list? modifiers)
    (any (lambda (x) 
           (eq? (strip-syntax x) 'static)) modifiers)
    modifiers))

(define (method static return class-name name args)
  (let* ((class-object   (find-class/or-error class-name))
         (return         (if (primitive? return) return (class->type (find-class/or-error return))))
         (get-method-id  (if static get-static-method-id get-method-id))
         (method         (get-method-id class-object (symbol->string name) (type-signature args return))))
     (if method 
       method
       (error "Method not found" class-name name))))

(define (call-new jclass jmethod jvalues)
  (if jmethod 
    (prepare-local-jobject (new-object jclass jmethod jvalues))
    (error 'call-new "method not found")))

;; convenient procedure to delegate invocation for the apropiate call (by modifier and return type)
(define (call-method modifiers object return-type jmethod jvalues)
  (if jmethod
    (if (static-signature? modifiers)
      (case return-type
        ((void)   (call-static-void-method    object jmethod jvalues))
        ((boolean)(call-static-boolean-method object jmethod jvalues))
        ((byte)   (call-static-byte-method    object jmethod jvalues))
        ((char)   (call-static-char-method    object jmethod jvalues))
        ((short)  (call-static-short-method   object jmethod jvalues))
        ((int)    (call-static-int-method     object jmethod jvalues))
        ((long)   (call-static-long-method    object jmethod jvalues))
        ((float)  (call-static-float-method   object jmethod jvalues))
        ((double) (call-static-double-method  object jmethod jvalues))
        (else     (prepare-local-jobject (call-static-object-method  object jmethod jvalues))))
      (case return-type
        ((void)   (call-void-method    object jmethod jvalues))
        ((boolean)(call-boolean-method object jmethod jvalues))
        ((byte)   (call-byte-method    object jmethod jvalues))
        ((char)   (call-char-method    object jmethod jvalues))
        ((short)  (call-short-method   object jmethod jvalues))
        ((int)    (call-int-method     object jmethod jvalues))
        ((long)   (call-long-method    object jmethod jvalues))
        ((float)  (call-float-method   object jmethod jvalues))
        ((double) (call-double-method  object jmethod jvalues))
        (else     (prepare-local-jobject (call-object-method  object jmethod jvalues)))))
    (error 'call-method "method not found")))

(define (make-jvalue-builder argument-types)
  (let* ((setters (map (lambda (type index)
                         (case type
                           ((boolean) (cut set-boolean-jvalue! <> index <>))
                           ((byte)    (cut set-byte-jvalue! <> index <>))   
                           ((char)    (cut set-char-jvalue! <> index <>))   
                           ((short)   (cut set-short-jvalue! <> index <>))  
                           ((int)     (cut set-int-jvalue! <> index <>))    
                           ((long)    (cut set-long-jvalue! <> index <>))   
                           ((float)   (cut set-float-jvalue! <> index <>))  
                           ((double)  (cut set-double-jvalue! <> index <>)) 
                           (else
                            (cut set-object-jvalue! <> index <>))))
                       argument-types
                       (iota (length argument-types)))))
    (lambda (args)
      (let ((jvalues (make-jvalue-array (length argument-types))))
        (for-each (lambda (type-setter! arg)
                    (if (string? arg)
                      (type-setter! jvalues (jstring arg)) ;; wont be GCed :(
                      (type-setter! jvalues arg)))
                  setters
                  args)
        jvalues))))

(define (make-caller argument-types modifiers return-type jmethod)
  (let ((jvalue-builder (make-jvalue-builder argument-types)))
    (lambda (args instance)
      (let* ((jvalues      (jvalue-builder args))
             (return-value (call-method modifiers instance return-type jmethod jvalues)))
        (free-jvalue-array jvalues)
        (check-jexception return-value)))))

(define (jlambda-method-imple* modifiers return-type class-type method-name argument-types)
  (let* ((static       (static-signature? modifiers)))
    (extend-procedure
      (let ((do-call (lambda (args instance)
                       (let* ((jmethod (method static return-type class-type method-name argument-types)))
                         ((make-caller argument-types modifiers return-type jmethod) args instance)))))
        (if static
          (lambda args 
            (do-call args (find-class/or-error class-type)))
          (lambda (object . args) 
            (do-call args object))))
      argument-types)))

(define (jlambda-method-imple modifiers return-type class-type method-name argument-types)
  (let* ((static       (static-signature? modifiers))
         (class-object (find-class/or-error class-type))
         (jmethod      (method static return-type class-type method-name argument-types))
         (caller       (make-caller argument-types modifiers return-type jmethod)))
    (extend-procedure
      (if static
        (lambda args 
          (caller args class-object))
        (lambda (object . args) 
          (caller args object)))
      argument-types)))

(define (jlambda-constructor-imple class-type argument-types)
  (let ((class-object (find-class/or-error class-type))
        (jmethod      (method #f 'void class-type '<init> argument-types))
        (jvalue-builder (make-jvalue-builder argument-types)))
    (extend-procedure
      (lambda args
        (let* ((jvalues      (jvalue-builder args))
               (return-value (call-new class-object jmethod jvalues)))
          (free-jvalue-array jvalues)
          (check-jexception return-value)))
      argument-types)))

;;TODO
(define (jlambda-methods types)
  (print types))

(define (field-accessor-for static accessor-type type)
	(if (eq? accessor-type 'get)
		(case type
			((boolean) (if static get-static-boolean-field get-boolean-field))
			((byte)    (if static get-static-byte-field    get-byte-field))
			((char)    (if static get-static-char-field    get-char-field))
			((short)   (if static get-static-short-field   get-short-field))
			((int)     (if static get-static-int-field     get-int-field))
			((long)    (if static get-static-long-field    get-long-field))
			((float)   (if static get-static-float-field   get-float-field))
			((double)  (if static get-static-double-field  get-double-field))
			(else      (if static get-static-object-field  get-object-field)))
		(case type
			((boolean) (if static set-static-boolean-field set-boolean-field))
			((byte)    (if static set-static-byte-field    set-byte-field))
			((char)    (if static set-static-char-field    set-char-field))
			((short)   (if static set-static-short-field   set-short-field))
			((int)     (if static set-static-int-field     set-int-field))
			((long)    (if static set-static-long-field    set-long-field))
			((float)   (if static set-static-float-field   set-float-field))
			((double)  (if static set-static-double-field  set-double-field))
			(else      (if static set-static-object-field  set-object-field)))))

(define (make-field-getter static type jclass jfield)
  (let ((prepare (lambda (v) 
                   (if (primitive? type) v (prepare-local-jobject v))))
        (accessor (field-accessor-for static 'get type)))
    (if static
      (lambda () 
        (prepare (accessor jclass jfield)))
      (lambda (object)
        (prepare (accessor object jfield))))))

(define (make-field-setter static type jclass jfield)
  (let ((accessor (field-accessor-for static 'set type)))
    (if static
      (lambda (value)
        (accessor jclass jfield value))
      (lambda (object value)
        (accessor object jfield value)))))

(define (jlambda-field-imple modifiers type class-name field-name)
  (let* ((field-name     (symbol->string field-name))
         (signature      (type-signature type))
         (static         (static-signature? modifiers))
         (jclass         (find-class/or-error class-name))
         (jfield         ((if static get-static-field get-field) jclass field-name signature))
         (field-fullname (string-append (symbol->string class-name) "." field-name)))
    (if jfield
      (getter-with-setter 
        (make-field-getter static type jclass jfield)
        (make-field-setter static type jclass jfield)
        field-fullname)
      (error 'field "field not found"))))

; convenient macro to access jlambda-method-imple
(define-syntax jlambda-method
  (syntax-rules ()
    ((_ modifiers return-type class-type method-name argument-types ...)
     (jlambda-method-imple 'modifiers 'return-type 'class-type 'method-name '(argument-types ...)))))

; convenient macro to access jlambda-method-imple*
(define-syntax jlambda-method*
  (syntax-rules ()
    ((_ modifiers return-type class-type method-name argument-types ...)
     (jlambda-method-imple* 'modifiers 'return-type 'class-type 'method-name '(argument-types ...)))))

; convenient macro to access jlambda-constructor-imple
(define-syntax jlambda-constructor
  (syntax-rules ()
    ((_ class argument-types ...)
     (jlambda-constructor-imple 'class '(argument-types ...)))))

; convenient macro to access jlambda-field-imple
(define-syntax jlambda-field
  (syntax-rules ()
    ((_ modifiers type class-name field-name)
     (jlambda-field-imple 'modifiers 'type 'class-name 'field-name))))

; convenient macro to access find-class/or-error
(define-syntax class
  (syntax-rules ()
    ((_ name)
     (find-class/or-error 'name))))

(define-syntax import-java-ns 
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%let*        (r 'let*))
             (%find-class  (r 'find-class))
             (%find-class* (r 'find-class*))
             (imports      (cadr x)))
        `(,%let* ((old-find-class     ,%find-class)
                  (old-import-table   (import-table))
                  (new-import-table   (make-import-table ',imports)))
                 (import-table (if old-import-table 
                                 (append old-import-table new-import-table)
                                 new-import-table)))))))

(define (jexception-trace exception)
  (let ((m (jlambda-method* (static) java.lang.String com.chicken_mobile.jni.ExceptionHelper traceAsString java.lang.Exception)))
    (jstring->string (m exception))))

(define (jexception-message exception)
  (let ((m (jlambda-method* #f java.lang.String java.lang.Exception getMessage)))
    (jstring->string (m exception))))

(define (jexception-type exception)
  (let ((m (jlambda-method* (static) java.lang.String com.chicken_mobile.jni.ExceptionHelper type java.lang.Exception)))
    (jstring->string (m exception))))

(define (make-condition exception)
  (let ((trace   (jexception-trace exception))
        (message (jexception-message exception))
        (type    (string->symbol (jexception-type exception))))
      (exception-clear)
      (make-composite-condition
        (make-property-condition 'exn)
        (make-property-condition 'java 'trace trace 'message message 'type type)
        (make-property-condition type))))

(define java-exception? 
  (condition-predicate 'java))

(define java-exception-message
  (condition-property-accessor 'java 'message #f))

(define java-exception-trace
  (condition-property-accessor 'java 'trace #f))

(define java-exception-type
  (condition-property-accessor 'java 'type #f))

(define java-condition-handler
  (let ((o (current-exception-handler)))
    (lambda (exception)
      (if (java-exception? exception)
        (let ((trace   (java-exception-trace exception))
              (message (java-exception-message exception)))
          (newline)
          (print trace)
          (abort (make-property-condition 'exn 'message "java-exception-handler returned")))
        (o exception)))))

(current-exception-handler java-condition-handler)

(define (check-jexception v)
  (let ((e (exception-occurred)))
    (if e
      (abort (make-condition e))
      v)))
