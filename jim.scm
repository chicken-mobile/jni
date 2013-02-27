(use jni lolevel expand-full moremacros srfi-13)

(define android-sdk-path "/home/paul/opt/adt-bundle-linux-x86/sdk")
(define android-platform-version 14)
(define android-platform-path 
  (string-append android-sdk-path "platforms/android-" (number->string android-platform-version) "/"))
(define android-platform-jar-path
  (string-append android-platform-path "android.jar"))

;;(jvm-init android-platform-jar-path)
(jvm-init)
(import-for-syntax jni)

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
			 ((java.lang.String java.lang.CharSequence java.lang.Object)
			  `(,%if (,%string? ,arg)
				 (,%set-object-jvalue! jvalues ,index (,%jstring ,arg)) ;; wont be GCed :(
				 (,%set-object-jvalue! jvalues ,index ,arg)))
			 (else
			  `(,%set-object-jvalue! jvalues ,index ,arg))))
		     argument-types
		     argument-names
		     (iota (length argument-types))))))))

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

       `(list ,@(map (lambda (argument-types)
		      `(jlambda-method ,modifiers ,class-type ,return-type ,method-name ,@argument-types))
		    argument-types-list))))))

(ppexpand* '(jlambda-methods (static) java.lang.String java.lang.String valueOf
														 ((boolean) (char) (#(char)) (#(char) int int)
																				(double) (float) (int) (long) (java.lang.Object))))


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
    (let* ((testo-methods (jlambda-methods (static) java.lang.String java.lang.String valueOf
					   ((boolean) (char) (#(char)) (#(char) int int)
					    (double) (float) (int) (long) (java.lang.Object))))
	   (useable-methods 
	    (filter (lambda (m)
		      (let ((margs (procedure-data m)))
			(let loop ((remaining-args args)
				   (remaining-margs margs))
			  (if (null? remaining-margs)
			      #t
			      (let ((arg (car remaining-args))
				    (marg (car remaining-margs)))
				(type-case arg
				  (jobject
				   (let* ((marg-class (find-class (mangle-class-name marg)))
					  (correct-class? (instance-of? arg marg-class)))
				     (delete-local-ref marg-class)
				     (if correct-class?
					 (loop (cdr (remaining-args))
					       (cdr (remaining-margs)))
					 #f)))

				  (boolean (eq? 'boolean marg))
				  (number (case marg
					    ((byte short int long double float
						   java.lang.BigDecimal java.lang.BigInteger)
					     (loop (cdr remaining-args)
						   (cdr remaining-margs)))
					    (else #f)))
				  (string
				   (let* ((marg-class (find-class (mangle-class-name marg)))
					  (arg-class (class java.lang.String))
					  (assignable? (assignable-from? arg-class marg-class)))
				     (delete-local-ref arg-class)
				     (delete-local-ref marg-class)
				     (if assignable?
					 (loop (cdr remaining-args)
					       (cdr remaining-margs))
					 #f)))))))))
		    (filter (lambda (m)
			      (let ((margs (procedure-data m)))
				(= (length margs) (length args))))
			    testo-methods))))
      
      (print "available method args:")
      (for-each (lambda (margs) (pp (procedure-data margs))) testo-methods)
      (print "useable method args:")
      (for-each (lambda (margs) (pp (procedure-data margs))) useable-methods)
      (print "-----------------")

      (apply (car useable-methods) args))))

(define (array->list* array-object)
  (map prepare-local-jobject (array->list array-object)))
(define (class* class-symbol)
  (prepare-local-jobject (find-class (mangle-class-name class-symbol))))
(define (super-class* class-object)
  (prepare-local-jobject (super-class class-object)))

(pp (jstring-value-of 1))
(print "-----------------\n\n")
(pp (testo-foo "muuuuuuuh"))
(print "-----------------\n\n")
(pp (testo-foo 111))
(print "-----------------\n\n")
