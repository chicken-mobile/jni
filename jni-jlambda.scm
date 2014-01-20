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

;; convenient procedure to delegate invocation for the apropiate call (by modifier and return type)
(define (get-method-caller modifiers return-type)
    (if (static-signature? modifiers)
      (case return-type
        ((void)    call-static-void-method)    
        ((boolean) call-static-boolean-method) 
        ((byte)    call-static-byte-method)    
        ((char)    call-static-char-method)    
        ((short)   call-static-short-method)   
        ((int)     call-static-int-method)     
        ((long)    call-static-long-method)    
        ((float)   call-static-float-method)   
        ((double)  call-static-double-method)  
        (else      (lambda (instance jmethod jvalues)
                     (prepare-jobject (call-static-object-method instance jmethod jvalues)))))
      (case return-type
        ((void)    call-void-method)    
        ((boolean) call-boolean-method) 
        ((byte)    call-byte-method)    
        ((char)    call-char-method)    
        ((short)   call-short-method)   
        ((int)     call-int-method)     
        ((long)    call-long-method)    
        ((float)   call-float-method)   
        ((double)  call-double-method)  
        (else      (lambda (instance jmethod jvalues)
                     (prepare-jobject (call-object-method instance jmethod jvalues)))))))

(define (make-jvalue-builder argument-types)
  (let* ((setters (map (lambda (type index)
                         (case type
                           ((boolean) (cut set-boolean-jvalue! <> index <>))
                           ((byte)    (cut set-byte-jvalue!    <> index <>))   
                           ((char)    (cut set-char-jvalue!    <> index <>))   
                           ((short)   (cut set-short-jvalue!   <> index <>))  
                           ((int)     (cut set-int-jvalue!     <> index <>))    
                           ((long)    (cut set-long-jvalue!    <> index <>))   
                           ((float)   (cut set-float-jvalue!   <> index <>))  
                           ((double)  (cut set-double-jvalue!  <> index <>)) 
                           (else      (cut set-object-jvalue!  <> index <>))))
                       argument-types
                       (iota (length argument-types)))))
    (lambda (args)
      (let ((jvalues (make-jvalue-array (length argument-types))))
        (for-each (lambda (type-setter! arg)
                      (type-setter! jvalues arg))
                  setters args)
        jvalues))))

(define (make-caller method-caller jvalue-builder jmethod)
    (lambda (args instance)
      (let* ((jvalues       (jvalue-builder args))
             (return-value  (method-caller instance jmethod jvalues)))
        (free-jvalue-array jvalues)
        (check-jexception return-value))))

(define (assert-method-exists modifiers return-type class-type method-name argument-types)
  (let* ((static   (static-signature? modifiers))
         (jmethod  (method static return-type class-type method-name argument-types)))
    (assert jmethod)))

(define (jlambda-method-imple modifiers return-type class-type method-name argument-types)
  (if (jni-env)
    (assert-method-exists modifiers return-type class-type method-name argument-types))
  (let* ((static         (static-signature? modifiers))
         (jvalue-builder (make-jvalue-builder argument-types))
         (method-caller  (get-method-caller modifiers return-type))
         (jmethod        #f)
         (caller         (lambda (args instance)
                           (if (not jmethod)
                             (set! jmethod (method static return-type class-type method-name argument-types)))
                           (if jmethod
                             ((make-caller method-caller jvalue-builder jmethod) args instance)
                             (error 'call-method "method not found")))))
    (if (static-signature? modifiers)
      (lambda args 
        (caller args (find-class/or-error class-type)))
      (lambda (object . args) 
        (caller args object)))))

(define (jlambda-constructor-imple class-type argument-types)
  (let* ((class-object   (find-class/or-error class-type))
         (jmethod        (method #f 'void class-type '<init> argument-types))
         (jvalue-builder (make-jvalue-builder argument-types))
         (caller         (make-caller call-new jvalue-builder jmethod)))
    (lambda args (caller args class-object))))

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
      (else      
        (if static 
          (lambda (jclass jfield) (prepare-jobject (get-static-object-field jclass jfield)))
          (lambda (object jfield) (prepare-jobject (get-object-field object jfield))))))
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

(define (make-field-accessor static accessor-type type jclass jfield)
  (let ((accessor (field-accessor-for static accessor-type type)))
    (if (eq? accessor-type 'get)
      (if static (cut accessor jclass jfield) (cut accessor <> jfield))
      (if static (cut accessor jclass jfield <>) (cut accessor <> jfield <>)))))

;; getter for static constant: the result is cached, and doesn't requiere a jni-env until getter invokation
(define (jlambda-constant-imple type class-name field-name)
  (let* ((field-name     (symbol->string field-name))
         (value          'empty))
    (lambda () 
      (if (eq? value 'empty)
        (let* ((signature      (type-signature (if (primitive? type) type (class->type (find-class/or-error type)))))
               (jclass         (find-class/or-error class-name))
               (jfield         (get-static-field jclass field-name signature))
               (new-value      ((field-accessor-for #t 'get type) jclass jfield)))
          (set! value new-value)))
      value)))

(define (jlambda-field-imple modifiers type class-name field-name)
  (let* ((field-name     (symbol->string field-name))
         (signature      (type-signature (if (primitive? type) type (class->type (find-class/or-error type)))))
         (static         (static-signature? modifiers))
         (jclass         (find-class/or-error class-name))
         (jfield         ((if static get-static-field get-field) jclass field-name signature))
         (field-fullname (string-append (symbol->string class-name) "." field-name)))
    (if jfield
      (getter-with-setter 
        (make-field-accessor static 'get type jclass jfield)
        (make-field-accessor static 'set type jclass jfield)
        field-fullname)
      (error 'field "field not found" field-name))))

; convenient macro to access jlambda-method-imple
(define-syntax jlambda-method
  (syntax-rules ()
    ((_ modifiers return-type class-type method-name argument-types ...)
     (jlambda-method-imple 'modifiers 'return-type 'class-type 'method-name '(argument-types ...)))))

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

; convenient macro to access jlambda-constant-imple
(define-syntax jlambda-constant
  (syntax-rules ()
    ((_ type class-name field-name)
     (jlambda-constant-imple 'type 'class-name 'field-name))))

; convenient macro to access find-class/or-error
(define-syntax class
  (syntax-rules ()
    ((_ name)
     (find-class/or-error 'name))))

(include "jni-method-selection.scm")

(define-syntax type:
  (syntax-rules ()
    ((_ type value)
     (cons 'type value))))

;; signature is (static? return-type arg-type..)
(define (jlambda-methods class-name method-name signatures)
  (let* ((methods       (generate-methods class-name method-name signatures))
         (method-finder (lambda (args/with-typehints)
                          (let ((method (find-method-match method-name methods args/with-typehints))
                                (args   (map (lambda (arg/with-typehints)
                                               (if (pair? arg/with-typehints)
                                                 (cdr arg/with-typehints)
                                                 arg/with-typehints)) args/with-typehints)))
                            (if method
                              (values (cddr method) args)
                              (error 'jlambda-methods 
                                     (format "cannot find method ~a with args: ~a" method-name args/with-typehints)))))))
    (lambda args
      (call-with-values (lambda () (method-finder args))
                        (lambda (method args)
                          (apply method args))))))

(define-syntax import-java-ns 
  (ir-macro-transformer
    (lambda (x i c)
      (let* ((%import-table   (i 'import-table))
             (imports         (cadr x)))
        `(let* ((old-import-table   (,%import-table))
                (new-import-table   (make-import-table ',imports)))
           (,%import-table (if old-import-table 
                             (append old-import-table new-import-table)
                             new-import-table)))))))

(define jexception-trace
  (let ((m (jlambda-method (static) java.lang.String com.chicken_mobile.jni.ExceptionHelper traceAsString java.lang.Exception)))
    (lambda (exception)
      (jstring->string (m exception)))))

(define jexception-message 
  (let ((m (jlambda-method #f java.lang.String java.lang.Exception getMessage)))
    (lambda (exception)
      (let ((message (m exception)))
        (and message (jstring->string message))))))

(define jexception-type
  (let ((m (jlambda-method (static) java.lang.String com.chicken_mobile.jni.ExceptionHelper type java.lang.Exception)))
    (lambda (exception)
      (jstring->string (m exception)))))

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

(define exception-cause
  (condition-property-accessor 'exn 'cause #f))

(define java-condition-handler
  (let ((o (current-exception-handler)))
    (lambda (exception)
      (let ((jexception (cond ((java-exception? exception) exception)
                              ((and (uncaught-exception? exception) 
                                    (java-exception? (uncaught-exception-reason exception)))
                                    (uncaught-exception-reason exception))
                              (else
                                #f))))
      (if jexception
        (let ((trace   (java-exception-trace jexception))
              (message (java-exception-message jexception)))
          (display (format "\n~a\n" trace) (current-error-port))
          (abort (make-property-condition 'exn 'message "java-exception-handler returned" 'cause jexception)))
        (o exception))))))

(current-exception-handler java-condition-handler)

(define (check-jexception v)
  (let ((e (exception-occurred)))
    (if e
      (java-condition-handler (make-condition e))
      v)))
