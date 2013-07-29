(module jni-jlambda-methods
(define-jlambda-methods jlambda-overloaded-method jlambda-method make-method-finder jlambda-methods-dispatcher jlambda-methods-list) 
(import chicken scheme matchable)
(use lolevel jni-lolevel jni-jlambda-method jni-jlambda-methods-selection)
(attach-thread)

(begin-for-syntax
 (require-library lolevel jni-lolevel jni-jlambda-method jni-jlambda-methods-selection))
(import-for-syntax lolevel jni-lolevel jni-jlambda-method jni-jlambda-methods-selection)

(define (make-method-finder method-name methods)
  (lambda (args/with-typehints)
    (let ((method (find-method-match methods args/with-typehints))
	  (args   (map (lambda (arg/with-typehints)
			 (if (pair? arg/with-typehints)
			     (cdr arg/with-typehints) arg/with-typehints)) 
		       args/with-typehints)))
      (if method
	  (values method args)
	  (error 'jlambda-methods
		 (format "cannot find method ~a with args: ~a" method-name args/with-typehints))))))

(define (jlambda-methods-dispatcher method-finder)
  (lambda args
    (call-with-values (lambda () (method-finder args))
      (lambda (method args)
	(match (procedure-data method)
	  ((class-object modifier return-type method-name arg-types ...)
	   (print (format "jni-jlambda-methods: dispatching to: ~A ~A ~A ~A~S"
			  (to-string class-object) modifier return-type method-name arg-types))))
	(apply method args)))))

(define-syntax jlambda-methods-list
  (syntax-rules ()
    ((_ class-object return-type method-name ((modifier arg-type ...) ...))
     (list (jlambda-non-overloaded-method class-object modifier return-type method-name arg-type ...) ...))))


(define-syntax jlambda-overloaded-method
  (syntax-rules ()
    ((_ class-object return-type method-name signatures)
     (let ((methods (jlambda-methods-list class-object return-type method-name signatures)))
       (let* ((name    (->string 'method-name))
	      (method-finder (make-method-finder name methods)))
	 (extend-procedure (jlambda-methods-dispatcher method-finder)
			   (list class-object 'return-type name)))))))

(define-syntax jlambda-method
  (syntax-rules ()
    ((_ class-object return-type method-name ((modifier (arg-types ...)) ...))
     (jlambda-overloaded-method class-object return-type method-name ((modifier arg-types ...) ...)))

    ((_ class-object modifier return-type method-name)
     (jlambda-non-overloaded-method class-object modifier return-type method-name))

    ((_ class-object return-type method-name 
	((static-arg-types ...) ...) ((nonstatic-arg-types ...) ...))
     (jlambda-overloaded-method class-object return-type method-name 
        ((static static-arg-types ...) ... (nonstatic  nonstatic-arg-types ...) ...)))

    ((_ class-object modifier return-type method-name (arg-types ...) ...)
     (jlambda-overloaded-method class-object return-type method-name ((modifier arg-types ...) ...)))

    ((_ class-object modifier return-type method-name arg-types ...)
     (jlambda-non-overloaded-method class-object modifier return-type method-name arg-types ...))))

(define-syntax define-jlambda-methods
  (syntax-rules ()
    ((_ class-object (name (signature ...)) ...)
     (begin
       (define name 
	 (jlambda-method class-object signature ...)) ...))))

)
