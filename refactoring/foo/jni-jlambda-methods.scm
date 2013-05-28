(module jni-jlambda-methods
*
(import chicken scheme extras matchable)
(use lolevel jni2-lolevel jni-types jni-jlambda-method  jni-jlambda-methods-selection jni-reflection)
(import-for-syntax matchable)

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
	  ((modifier class-object return-type method-name arg-types ...)
	   (print (format "jni-jlambda-methods: dispatching to: ~A ~A ~A ~A~S"
			  (class-name class-object) modifier return-type method-name arg-types))))
	(apply method args)))))

(define-syntax jlambda-methods-list
  (syntax-rules ()
    ((_ modifier class-object return-type method-name ((arg-type ...) ...))
     (list (extend-procedure (jlambda-non-overloaded-method modifier class-object return-type method-name arg-type ...)
			     `(modifier ,class-object return-type method-name arg-type ...)) ...))))

(define-syntax jlambda-overloaded-method
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type method-name signatures)
	`(let* ((methods (jlambda-methods-list ,modifier ,class-object ,return-type ,method-name ,signatures))
		(method-finder (make-method-finder ,(->string (strip-syntax method-name)) methods)))
	   (extend-procedure (jlambda-methods-dispatcher method-finder)
			     (list ',modifier ,class-object ',return-type ,(->string method-name)))))))))
)
