(use jni lolevel expand-full)

(define android-sdk-path "/opt/google/android/sdk/")
(define android-platform-version 14)
(define android-platform-path 
  (string-append android-sdk-path "platforms/android-" (number->string android-platform-version) "/"))
(define android-platform-jar-path
  (string-append android-platform-path "android.jar"))

;;(jvm-init android-platform-jar-path)
(jvm-init)
(import-for-syntax jni)

(define-record jobject-meta)
(mutate-procedure ##sys#pointer->string
  (lambda (old)
    (lambda args
      (let ((arg (car args)))
	(if (jobject-meta? (pointer-tag arg))
	    (let* ((object-class (get-object-class arg))
		   (jobject-string (format "#<jref <~A> ~A>" (to-string object-class) (to-string arg))))
	      (delete-local-ref object-class)
	      jobject-string)
	    (apply old args))))))

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
	   (%string? (r 'string?))
	   (%jstring (r 'jstring))
	   
	   (argument-types (cadr x))
	   (argument-names (cddr x)))

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
			 ((java.lang.String java.lang.CharSequence)
			  `(,%if (,%string? ,arg)
				 (,%set-object-jvalue! jvalues ,index (,%jstring ,arg)) ;; wont be GCed :(
				 (,%set-object-jvalue! jvalues ,index ,arg)))
			 (else
			  `(,%set-object-jvalue! jvalues ,index ,arg))))
		     argument-types
		     argument-names
		     (iota (length argument-types))))))))

(define (prepare-local-jobject jobject)
  (set-finalizer! (tag-pointer jobject (make-jobject-meta)) delete-local-ref))

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

(define-syntax jlambda-method
  (er-macro-transformer
   (lambda (x r c)
     (let* ((%lambda (r 'lambda))
	    (%let (r 'let*))
	    (%find-class (r 'find-class))
	    (%get-method-id (r 'get-method-id))
	    (%get-static-method-id (r 'get-static-method-id))
	    (%make-jvalue-array (r 'make-jvalue-array))
	    (%free-jvalue-array (r 'free-jvalue-array))
	    (%delete-local-ref (r' delete-local-ref))
	    (%delete-global-ref (r 'delete-global-ref))
	    (%if (r 'if))
	    (%exception-check (r 'exception-check))
	    (%error (r 'error))
	    (%method-id->Method (r 'method-id->Method))
	    (%format (r 'format))
	    

	    (modifiers (cadr x))
	    (class-type (caddr x))
	    (return-type (cadddr x))
	    (method-name (car (cddddr x)))
	    (argument-types  (cdr (cddddr x)))

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
	 ',argument-types)))))

(ppexpand* '(jlambda-method #f java.lang.String boolean contains java.lang.CharSequence))
(ppexpand* '(jlambda-method (static) java.lang.String java.lang.String valueOf int))

(define jstring-contains
  (jlambda-method #f java.lang.String boolean contains java.lang.CharSequence))
(define jstring-value-of
  (jlambda-method (static) java.lang.String java.lang.String valueOf int))


(import-for-syntax srfi-1)

(define-syntax jlambda-methods
  (er-macro-transformer
   (lambda (x r c)
     (let ((modifiers (cadr x))
	   (class-type (caddr x))
	   (return-type (cadddr x))
	   (method-name (car (cddddr x)))
	   (argument-types-list  (cadr (cddddr x))))
       ;; (pp modifiers)
       ;; (pp class-type)
       ;; (pp return-type)
       ;; (pp method-name)
       ;; (pp argument-types-list)

       ;; same length ?
       ;; same type ?
       ;; inhertied type ?
       ;; error

       (map (lambda (argument-types)
	      `(',argument-types (jlambda-method ,modifiers ,class-type ,return-type ,method-name ,@argument-types)))
	    argument-types-list)))))

(ppexpand* '(jlambda-methods (static) java.lang.String java.lang.String valueOf
			     ((int) (long) (float) (double))))


;; zuerst wird gefiltert welche methoden überhaupt in frage kämen 
;;; also ob gleiche länge und "ungefähr" gleicher typ
;; sollte einer der argumente von Number erben wird die methode mit dem passensten typen gewählt
;;;; das heißt wenn die zahl die übergeben wurde exact ist werden die methoden in dieser reihenfolge absteigend priorisiert 
;;;;; long int short char byte BigInteger
;;;; ist das argument inexect ist die reihenfole folgende
;;;;; double float BigDecimal
;; solte das übergebene argument ein string sein werden String und CharSequence  erlaubt und in der reihenfolge gewichtet
;;;; CharBuffer, Segment, String, StringBuffer und StringBuilder implementieren alle CharSequence
;; sollte das übergebene argument ein pointer sein und eine gültige jobject reference sein:
;;; wird überprüft ob der typ eine superclasse des methoden arguments ist oder genau die klasse hat die gesucht wird
;;; hierbei sollte die jeweils konkretere implementierung (das was am nächsten dran ist) am höchsten gewichtet werden
;; boolean ist eindeutig
;; jede methode bekommt eine gewichtung entsprechend der prioritäten die mit den gewichten der anderen argumente addiert wird
;; die methode mit dem besten match gewinnt sollte keine gefunden werden wird eine condition signalisiert


(define testo-foo
  (lambda args
    (let ((testo-methods (jlambda-methods (static) java.lang.String java.lang.String valueOf
					  ((int) (long) (float) (double) (java.lang.String)))))
      (filter (lambda (m)
		(let ((margs (procedure-data m)))
		  (let loop ((remaining-args args)
			     (remaining-margs margs))
		    (if (null? remaining-margs)
			#t
			(let ((arg (car remaining-args))
			      (marg (car remaining-margs)))
			  (case marg
			    ((long int short double float java.lang.BigInteger java.lang.BigDecimal)
			     (if (number? arg)
				 (loop (cdr remaining-args)
				       (cdr remaining-margs))
				 #f))
			    ((java.lang.String)
			     (if (string? arg)
				 (loop (cdr remaining-args)
				       (cdr remaining-margs))
				 #f))
			    (else
			     #f))
			  )))))
       (filter (lambda (m)
		 (let ((margs (procedure-data m)))
		   (= (length margs) (length args))))
	       testo-methods)))))


(pp (jstring-value-of 1))
(pp (testo-foo "foo"))
(pp (testo-foo 111))







