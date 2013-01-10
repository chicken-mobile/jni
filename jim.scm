#!/usr/local/bin/csi -s
(use jni list-utils expand-full)

(define android-sdk-path "/opt/google/android/sdk/")
(define android-platform-version 14)
(define android-platform-path 
  (string-append android-sdk-path "platforms/android-" (number->string android-platform-version) "/"))
(define android-platform-jar-path
  (string-append android-platform-path "android.jar"))

;;(jvm-init android-platform-jar-path)
(jvm-init)

(import-for-syntax list-utils)
(import-for-syntax jni)


;; (jlambda (java.lang.String (contains boolean java.lang.CharSequence)))


#;
(define-syntax call-method
  (syntax-rules (void jobject boolean byte char short int long float double)
    ((_ void object method-id jvalues)
     (call-void-method object method-id jvalues))
    ((_ jobject object method-id jvalues)
     (call-object-method object method-id jvalues))
    ((_ boolean object method-id jvalues)
     (call-boolean-method object method-id jvalues))
    ((_ byte object method-id jvalues)
     (call-byte-method object method-id jvalues))
    ((_ char object method-id jvalues)
     (call-char-method object method-id jvalues))
    ((_ short object method-id jvalues)
     (call-short-method object method-id jvalues))
    ((_ int object method-id jvalues)
     (call-int-method object method-id jvalues))
    ((_ long object method-id jvalues)
     (call-long-method object method-id jvalues))
    ((_ float object method-id jvalues)
     (call-float-method object method-id jvalues))
    ((_ double object method-id jvalues)
     (call-double-method object method-id jvalues))))

(define-syntax jlambda
  (syntax-rules (void boolean byte char short int long float double)
    ((_ class-name return-type method-name argument ...)
     (lambda (object . args)
       (let ((jlambda-method (method class-name return-type method-name argument ...)))
	 (call-method jobject object jlambda-method (jvalue-zip (argument ...) args)))))))


;;(ppexpand* '(jvalue-zip (java.lang.CharSequence boolean) "foo" #f))
;;(ppexpand* '(jlambda java.lang.String boolean contains java.lang.CharSequence))


(define throwable-get-message
  (lambda (object . args)
    (let* ((class-object   (find-class "java/lang/Exception"))
	   (jlambda-method (get-method-id class-object "getMessage" "()Ljava/lang/String;"))
	   (jvalues (make-jvalue-array 0)))

      (if (exception-check)
	  (error 'jstring-contains (to-string (exception-occurred)))
	  (set-finalizer! (call-object-method object jlambda-method jvalues) delete-local-ref)))))

(define throwable-print-stack-trace
  (lambda (object . args)
    (let* ((class-object   (find-class "java/lang/Exception"))
	   (jlambda-method (get-method-id class-object "printStackTrace" "()V"))
	   (jvalues (make-jvalue-array 0)))

      (if (exception-check)
	  (error 'jstring-contains (to-string (exception-occurred)))
	  (set-finalizer! (call-void-method object jlambda-method jvalues) delete-local-ref delete-local-ref)))))


(define jstring-contains
  (lambda (object . args)
    (let* ((class-object   (find-class "java/lang/String"))
	   (jlambda-method (get-method-id class-object "contains" "(Ljava/lang/CharSequence;)Z"))
	   (jvalues (make-jvalue-array 1)))
      (set-object-jvalue! jvalues 0 (list-ref args 0))
      
      (free-jvalue-array jvalues)
      (delete-local-ref class-object)

      (if (exception-check)
	  (error 'jstring-contains (to-string (exception-occurred)))
	  (set-finalizer! (call-boolean-method object jlambda-method jvalues) delete-local-ref)))))


(define-syntax jvalue-zip
  (er-macro-transformer
   (lambda (x r c)
     (let ((%let (r 'let))
	   (%make-jvalue-array (r 'make-jvalue-array))
	   (%set-boolean-jvalue! (r 'set-boolean-jvalue!))
	   (%set-byte-jvalue! (r 'set-byte-jvalue!))
	   (%set-char-jvalue! (r 'set-char-jvalue!))
	   (%set-int-jvalue! (r 'set-int-jvalue!))
	   (%set-long-jvalue! (r 'set-long-jvalue!))
	   (%set-float-jvalue! (r 'set-float-jvalue!))
	   (%set-double-jvalue! (r 'set-double-jvalue!))
	   (%set-object-jvalue! (r 'set-object-jvalue!))
	   (%if (r 'if))
	   (%car (r 'car))
	   
	   (argument-types (cadr x))
	   (arguments (cddr x)))
       `(,%let ((jvalues (,%make-jvalue-array ,(length argument-types))))
	       ,(car (fold (lambda (argument-type setters)
			     (cons (let* ((index (length setters))
					  (arg `(list-ref ,(car arguments) ,index)))
				     (case argument-type
				       ((boolean) `(set-boolean-jvalue! jvalues ,index ,arg))
				       ((byte) `(set-byte-jvalue! jvalues ,index ,arg))
				       ((char) `(set-char-jvalue! jvalues ,index ,arg))
				       ((short) `(set-short-jvalue! jvalues ,index ,arg))
				       ((int) `(set-int-jvalue! jvalues ,index ,arg))
				       ((long) `(set-long-jvalue! jvalues ,index ,arg))
				       ((float) `(set-float-jvalue! jvalues ,index ,arg))
				       ((double) `(set-double-jvalue! jvalues ,index ,arg))
				       ((java.lang.String)
					`(if (string? arg)
					     (set-object-jvalue! jvalues ,index (jstring ,arg)) ;; wont be GCed :(
					     (set-object-jvalue! jvalues ,index ,arg)))
				       ((java.lang.CharSequence)
					`(if (string? arg)
					     (set-object-jvalue! jvalues ,index (jstring ,arg)) ;; wont be GCed :(
					     (set-object-jvalue! jvalues ,index ,arg)))
				       (else
					`(set-object-jvalue! jvalues ,index ,arg))))
				   setters))
			   '()
			   argument-types))
	       jvalues)))))

(define-syntax call-method
  (er-macro-transformer
   (lambda (x r c)
     (let ((%call-void-method (r 'call-void-method))
	   (%call-object-method (r 'call-object-method))
	   (%call-byte-method (r 'call-byte-method))
	   (%call-char-method (r 'call-char-method))
	   (%call-short-method (r 'call-short-method))
	   (%call-int-method (r 'call-int-method))
	   (%call-long-method (r 'call-long-method))
	   (%call-float-method (r 'call-float-method))
	   (%call-double-method (r 'call-double-method))
	   
	   (return-type (cadr x))
	   (object  (caddr x))
	   (jmethod (cadddr x))
	   (jvalues (cadddr x)))
       
       (case return-type
	 ((void)   `(,%call-void-method   ,object ,jmethod ,jvalues))
	 ((byte)   `(,%call-byte-method   ,object ,jmethod ,jvalues))
	 ((char)   `(,%call-char-method   ,object ,jmethod ,jvalues))
	 ((short)  `(,%call-short-method  ,object ,jmethod ,jvalues))
	 ((int)    `(,%call-int-method    ,object ,jmethod ,jvalues))
	 ((long)   `(,%call-long-method   ,object ,jmethod ,jvalues))
	 ((float)  `(,%call-float-method  ,object ,jmethod ,jvalues))
	 ((double) `(,%call-double-method ,object ,jmethod ,jvalues))
	 (else     `(,%call-object-method ,object ,jmethod ,jvalues)))))))

;; (jlambda java.lang.String boolean contains java.lang.CharSequence)
(define-syntax jlambda
  (er-macro-transformer
   (lambda (x r c)
     (let ((%lambda (r 'lambda))
	   (%let (r 'let))
	   (%find-class (r 'find-class))
	   (%get-method-id (r 'get-method-id))
	   (%make-jvalue-array (r 'make-jvalue-array))
	   (%free-jvalue-array (r 'free-jvalue-array))
	   (%delete-local-ref (r' delete-local-ref))
	   (%delete-global-ref (r 'delete-global-ref))
	   (%if (r 'if))
	   (%exception-check (r 'exception-check))
	   (%error (r 'error))
	   (%method-id->Method (r 'method-id->Method))
	   (%format (r 'format))
	   

	   (class-type (cadr x))
	   (return-type (caddr x))
	   (method-name (cadddr x))
	   (argument-types (cddddr x)))

       `(,%lambda (object . args)
		  (,%let ((class-object (,%find-class ,(mangle-class-name class-type)))
			  (jmethod (,%get-method-id class-object ,(symbol->string method-name) (type-signature ,argument-types ,return-type)))
			  (jvalues (jvalue-zip ,argument-types args))
			  (return-value (call-method ,return-type jmethod jvalues)))
			 
			 (,%if (,%exception-check)
			       (,%error 
				(,%let ((rmethod (,%method-id->Method jmethod))
					(method-name (,%format "%s.%s" (to-string class-object) (to-string rmethod))))
				       (,%delete-local-ref rmethod)
				       method-name)))))))))

(ppexpand* '(jlambda java.lang.String boolean contains java.lang.CharSequence))


;;((jlambda java.lang.String boolean contains java.lang.CharSequence) (jstring "foobar") (jstring "bar"))
(print (jstring-contains (jstring "foobar") (jstring "bar")))
(jvm-destroy (java-vm))
