(define-syntax check-exists
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%let      (r 'let))
             (%error    (r 'error))
             (%format   (r 'format))
             (%if       (r 'if))
             (v         (cadr x))
             (location  (caddr x))
             (message   (cadddr x))
             (args      (cddddr x)))
        `(,%let ((value ,v))
                (,%if value value (,%error ',location ,message '(,@args))))))))

(define (find-class/or-error name)
  (let ((class (find-class (mangle-class-name name))))
    (if class
      class
      (error 'find-class/or-error "class not found" name))))

(define-for-syntax (static-signature? modifiers)
	(if (list? modifiers)
		(any (lambda (x) 
					 (eq? (strip-syntax x) 'static)) modifiers)
		modifiers))

(define-syntax class
  (ir-macro-transformer
    (lambda (x i c)
      (let ((name (strip-syntax (cadr x))))
        `(find-class (mangle-class-name ',name))))))

(define-syntax class/or-error
  (er-macro-transformer
    (lambda (x r c)
      (let ((%check-exists   (r 'check-exists))
            (%class          (r 'class))
            (args            (cdr x)))
        `(,%check-exists (,%class ,@args) class "class not found" ,@args)))))

(define to-string
  (lambda (object)
    (let* ((Object.toString/method (get-method-id (class java.lang.Object) "toString" "()Ljava/lang/String;"))
           (String/instance (call-object-method object Object.toString/method #f))
           (string (jstring->string String/instance)))
      (delete-local-ref String/instance) string)))

(define-syntax method* 
  (er-macro-transformer
    (lambda (x r c)
     (let* ((%class            (r 'class/or-error))
            (%let*             (r 'let*))
            (%class->type   (r 'class->type))
            (fn                (cadr x))
            (return            (caddr x))
            (class-name        (cadddr x))
            (name              (car (cddddr x)))
            (args              (cdr (cddddr x))))
       `(,%let* ((class-object (,%class ,class-name))
                 (return       ,(if (primitive? return) `',return `(,%class->type (,%class ,return))))
                 (return-value (,fn class-object (symbol->string ',name) 
                                    (type-signature ',args return))))
               return-value)))))

(define-syntax method
  (syntax-rules ()
    ((_ args ...)
     (check-exists (method* get-method-id args ...) method "Method not found" args ...))))

(define-syntax static-method
  (syntax-rules ()
    ((_ args ...)
     (check-exists (method* get-static-method-id args ...) method "Static method not found" args ...))))

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
                          (parameterize ((jni-env env)) . ,(cdddr x)))))))

(define (call-new jclass jmethod jvalues)
  (if jmethod 
    (prepare-local-jobject (new-object jclass jmethod jvalues))
    (error 'call-new "method not found")))

;; convenient procedure to delegate invocation for the apropiate call (by modifier and return type)
(define-syntax call-method
  (er-macro-transformer
    (lambda (x r c)
      (let ((%if                         (r 'if))
            (%error                      (r 'error))
            (%call-static-boolean-method (r 'call-static-boolean-method))
            (%call-static-void-method    (r 'call-static-void-method))
            (%call-static-object-method  (r 'call-static-object-method))
            (%call-static-byte-method    (r 'call-static-byte-method))
            (%call-static-char-method    (r 'call-static-char-method))
            (%call-static-short-method   (r 'call-static-short-method))
            (%call-static-int-method     (r 'call-static-int-method))
            (%call-static-long-method    (r 'call-static-long-method))
            (%call-static-float-method   (r 'call-static-float-method))
            (%call-static-double-method  (r 'call-static-double-method))
            (%call-void-method           (r 'call-void-method))
            (%call-boolean-method        (r 'call-boolean-method))
            (%call-object-method         (r 'call-object-method))
            (%call-byte-method           (r 'call-byte-method))
            (%call-char-method           (r 'call-char-method))
            (%call-short-method          (r 'call-short-method))
            (%call-int-method            (r 'call-int-method))
            (%call-long-method           (r 'call-long-method))
            (%call-float-method          (r 'call-float-method))
            (%call-double-method         (r 'call-double-method))
            (%prepare-local-jobject      (r 'prepare-local-jobject))
            (modifiers                   (cadr x))
            (object                      (caddr x))
            (return-type                 (cadddr x))
            (jmethod                     (car (cddddr x)))
            (jvalues                     (cadr (cddddr x))))
        `(,%if ,jmethod
            ,(if (static-signature? modifiers)
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
                 (else     `(,%prepare-local-jobject (,%call-object-method  ,object ,jmethod ,jvalues)))))
            (,%error 'call-method "method not found"))))))

(define-syntax jvalue-zip
  (er-macro-transformer
    (lambda (x r c)
      (let ((%let                 (r 'let))
            (%error               (r 'error))
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

(define-for-syntax (build-argument-names argument-types)
  (map (lambda (arg-count)
         (string->symbol (format "a~A" arg-count)))
       (iota (length argument-types) 1 1)))

(define-syntax build-arguments-jvalue 
  (er-macro-transformer
    (lambda (x r c)
      (let ((argument-types (cadr x))
            (argument-names (caddr x)))
      (if (null? argument-types) #f `(jvalue-zip ,argument-types ,@argument-names))))))

;; (jlambda-method [modifiers] CLASS RETURN-TYPE METHOD-NAME ARGS...) => lambda
(define-syntax jlambda-method*
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%lambda               (r 'lambda))
             (%let                  (r 'let*))
             (%class                (r 'class))
             (%method               (r 'method))
             (%static-method        (r 'static-method))
             (%make-jvalue-array    (r 'make-jvalue-array))
             (%free-jvalue-array    (r 'free-jvalue-array))
             (modifiers             (cadr x))
             (return-type           (caddr x))
             (class-type            (cadddr x))
             (method-name           (car (cddddr x)))
             (argument-types        (cdr (cddddr x)))
             (static                (static-signature? modifiers)))
        (let* ((argument-names  (build-argument-names argument-types)))
          `(extend-procedure
             (,%lambda (,@(append (if static '() '(object)) argument-names))
                       (,%let ((class-object (,%class ,class-type))
                               (jmethod      (,(if static %static-method %method) 
                                               ,return-type ,class-type ,method-name ,@argument-types))
                               (jvalues      (build-arguments-jvalue ,argument-types ,argument-names))
                               (return-value (call-method ,modifiers ,(if static 'class-object 'object) 
                                                          ,return-type jmethod jvalues)))
                              (,%free-jvalue-array jvalues)
															(check-jexception return-value)))
             ',argument-types))))))

(define-syntax jlambda-method
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%lambda               (r 'lambda))
             (%let                  (r 'let*))
             (%class                (r 'class))
             (%method               (r 'method))
             (%static-method        (r 'static-method))
             (%make-jvalue-array    (r 'make-jvalue-array))
             (%free-jvalue-array    (r 'free-jvalue-array))
             (modifiers             (cadr x))
             (return-type           (caddr x))
             (class-type            (cadddr x))
             (method-name           (car (cddddr x)))
             (argument-types        (cdr (cddddr x)))
             (static                (static-signature? modifiers)))
        (let* ((argument-names  (build-argument-names argument-types)))
          `(,%let ((class-object (,%class ,class-type))
                   (jmethod      (,(if static %static-method %method) 
                                   ,return-type ,class-type ,method-name ,@argument-types)))
                  (extend-procedure
                    (,%lambda (,@(append (if static '() '(object)) argument-names))
                              (,%let (
                                      (jvalues      (build-arguments-jvalue ,argument-types ,argument-names))
                                      (return-value (call-method ,modifiers ,(if static 'class-object 'object) 
                                                                 ,return-type jmethod jvalues)))
                                     (,%free-jvalue-array jvalues)
																		(check-jexception return-value)))
                    ',argument-types)))))))

(define-syntax jlambda-constructor
  (er-macro-transformer
    (lambda (x r c)
      (let ((%lambda                (r 'lambda))
            (%let                   (r 'let*))
            (%class                 (r 'class/or-error))
            (%method                (r 'method))
            (%make-jvalue-array     (r 'make-jvalue-array))
            (%free-jvalue-array     (r 'free-jvalue-array))
            (%check-jexception      (r 'check-jexception))
            (%call-new              (r 'call-new))
            (class-type             (cadr x))
            (argument-types         (cddr x)))
        (let* ((argument-names  (build-argument-names argument-types)))
          `(,%let ((class-object (,%class ,class-type))
                   (jmethod      (,%method void ,class-type <init> ,@argument-types)))
              (extend-procedure
                (,%lambda (,@argument-names)
                          (,%let ((jmethod      (,%method void ,class-type <init> ,@argument-types))
                                  (jvalues      (build-arguments-jvalue ,argument-types ,argument-names))
                                  (return-value (,%call-new class-object jmethod jvalues)))
                                 (,%free-jvalue-array jvalues)
                                 (check-jexception return-value)))
                ',argument-types)))))))

;TODO: transition procedure, primitive? and primitive2? should be merged in one
(define (primitive2? type)
  (member type '(void boolean byte char short int long float double)))

;TODO: idem
(define (static-signature2? modifiers)
	(if (list? modifiers)
		(any (lambda (x) 
					 (eq? (strip-syntax x) 'static)) modifiers)
		modifiers))

(define (field-accessor-for static accessor-type type)
  (let* ((accessor-type (symbol->string accessor-type))
         (modifier      (if static "static-" ""))
         (type          (if (primitive2? type) (symbol->string type) "object"))
         (accessor-proc (string->symbol (string-append accessor-type "-" modifier type "-field"))))
    (eval accessor-proc)))

(define (make-field-getter static type jclass jfield)
	(let ((prepare (lambda (v) 
									 (if (primitive2? type) v (prepare-local-jobject v)))))
		(if static
			(lambda () 
				(prepare ((field-accessor-for #t 'get type) jclass jfield)))
			(lambda (object)
				(prepare ((field-accessor-for #f 'get type) object jfield))))))

(define (make-field-setter static type jclass jfield)
	(if static
		(lambda (value)
			((field-accessor-for #t 'set type) jclass jfield value))
		(lambda (object value)
			((field-accessor-for #f 'set type) object jfield value))))

; jlambda-field implementation
(define (jlambda-field* modifiers type class-name field-name)
  (let* ((field-name     (symbol->string field-name))
         (signature      (type-signature type))
         (static         (static-signature2? modifiers))
         (jclass         (find-class/or-error class-name))
         (jfield         ((if static get-static-field get-field) jclass field-name signature))
         (field-fullname (string-append (symbol->string class-name) "." field-name)))
    (if jfield
      (getter-with-setter 
        (make-field-getter static type jclass jfield)
        (make-field-setter static type jclass jfield)
        field-fullname)
      (error 'field "field not found"))))

; convenient macro to access jlambda-field*
(define-syntax jlambda-field
  (syntax-rules ()
  	((_ modifiers type class-name field-name)
  	 (jlambda-field* 'modifiers 'type 'class-name 'field-name))))

(define (join-class-pkg pkg class)
  (symbol-append (string->symbol pkg) '|.| class))

(define (make-import-table imports)
  (let loop ((imports imports)
             (classes '())
             (pkgs    '()))
    (if (null? imports)
      (append classes (list (cons '* pkgs)))
      (let* ((import   (car imports))
             (pkg      (symbol->string (car import)))
             (s-class  (cadr import)))
        (if (eq? s-class '*)
          (loop (cdr imports) classes (cons pkg pkgs))
          (loop (cdr imports) 
                (if (list? s-class)
                  (append (map (lambda (x) (cons x (join-class-pkg pkg x))) s-class) classes)
                  (cons (cons s-class (join-class-pkg pkg s-class)) classes))
                pkgs))))))

(define (find-class/by-pkgs find-class class pkgs)
  (call/cc (lambda (found)
             (fold (lambda (import _)
                     (let* ((class-name   (join-class-pkg import class))
                            (mangled-name (mangle-class-name class-name))
                            (r            (find-class mangled-name)))
                       (if r (found r) #f)))
                   '() pkgs))))

(define (find-class* import-table find-class class)
  (let ((s-class (string->symbol class)))
    (or (find-class class)
        (let ((import (assq s-class import-table)))
          (and import (find-class (mangle-class-name (cdr import)))))
        (let ((pkgs (cdr (assq '* import-table))))
          (and (not (null? pkgs))
               (find-class/by-pkgs find-class s-class pkgs))))))

(define-syntax import-java-ns 
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%let*        (r 'let*))
             (%find-class  (r 'find-class))
             (%find-class* (r 'find-class*))
             (imports      (cadr x))
             (body         (cddr x)))
        `(,%let* ((old-find-class ,%find-class)
                  (import-table   (make-import-table ',imports)))
                 (set! find-class (lambda (c) (,%find-class* import-table old-find-class c)))
                 ,@body
                 (set! find-class old-find-class))))))

(define (jexception-trace exception)
  (let ((m (jlambda-method* (static) java.lang.String com.chicken_mobile.jni.ExceptionHelper traceAsString java.lang.Exception)))
    (jstring->string (m exception))))

(define (jexception-message exception)
  (let ((m (jlambda-method* #f java.lang.String java.lang.Exception getMessage)))
    (jstring->string (m exception))))

(define (jexception-type exception)
  (let ((m (jlambda-method (static) java.lang.String com.chicken_mobile.jni.ExceptionHelper type java.lang.Exception)))
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
