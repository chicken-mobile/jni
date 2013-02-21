; symbol => jclass
(define-syntax class
  (ir-macro-transformer
   (lambda (x i c)
     (let ((name (mangle-class-name (strip-syntax (cadr x)))))
       `(find-class ,name)))))

(define-syntax method*
  (syntax-rules ()
    ((_ fn class-name return name args ...)
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
     `(,(r 'method) ,(cadr x) void <init> . ,(cddr x)))))

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

;; (jlambda-method [modifiers] CLASS RETURN-TYPE METHOD-NAME ARGS...) => lambda
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
            (%error (r 'error))
            (%method-id->Method (r 'method-id->Method))
            (%format (r 'format))

            (modifiers (cadr x))
            (class-type (caddr x))
            (return-type (cadddr x))
            (method-name (car (cddddr x))))
        (let* ((argument-types  (cdr (cddddr x)))
               (argument-names (map (lambda (arg-count)
                                      (string->symbol (format "a~A" arg-count)))
                                    (iota (length argument-types) 1 1))))

          `(extend-procedure
             (,%lambda (,@(append (if modifiers '() '(object)) argument-names))
                       (,%let ((class-object (,%find-class ,(mangle-class-name class-type)))
                               (jmethod (,(if modifiers %get-static-method-id %get-method-id ) 
                                          class-object ,(symbol->string method-name) 
                                          (type-signature ,argument-types ,return-type)))
                               (jvalues ,(if (null? argument-types) #f `(jvalue-zip ,argument-types ,@argument-names)))
                               (return-value (call-method ,modifiers ,(if modifiers 'class-object 'object) ,return-type jmethod jvalues)))
                              (,%if (,%exception-check)
                                    (,%error 'fooooo)
                                    return-value)))
             ',argument-types))))))

(define-syntax jimport
	(er-macro-transformer
		(lambda (x r c)
			(pp x)
			(let ((object-class (class* (cadr x))))
				(pp object-class)
				`(list ,@(map (lambda (method)
												(let ((class-name (string->symbol (string-drop (to-string object-class) 6)))
															(modifier (Method.getModifiers method))
															(name (Method.getName method))
															(return-type (Method.getReturnType method))
															(params (array->list* (Method.getParameterTypes method))))

													(let ((modifier-symbols
																	(let ((modifier-symbols '()))
																		(if (not (public? modifier))
																			(set! modifier-symbols (cons 'private modifier-symbols)))
																		(if (static? modifier)
																			(set! modifier-symbols (cons 'static modifier-symbols)))			     			      
																		modifier-symbols)))

														(let ((jlambda-def 
																		`(,class-name
																			 ,(string->symbol (let ((foo (to-string return-type)))
																													(if (string-prefix? "class " foo)
																														(string-drop foo 6)
																														foo)) ) 
																			 ,(string->symbol (jstring->string name)) 
																			 ,@(map (lambda (param)
																								(string->symbol (let ((foo (to-string param)))
																																	(if (string-prefix? "class " foo)
																																		(string-drop foo 6)
																																		foo))))
																							params))))
															(cons 'jlambda-method
																		(if (null? modifier-symbols)
																			(cons #f jlambda-def)
																			(cons modifier-symbols jlambda-def))))
														)))
											(array->list* (Class.getDeclaredMethods object-class))))))))
