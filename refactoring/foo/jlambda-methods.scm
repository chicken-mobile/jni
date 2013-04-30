(use jni-lolevel lolevel moremacros)
(jvm-init)
(include "class.scm")
(include "jlambda-method.scm")
(include "jlambda-methods-selection.scm")

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
	(apply method args)))))

(define (jlambda-methods class-name method-name signatures)
  (let* ((methods       (generate-methods class-name method-name signatures))
         (method-finder (make-method-finder method-name methods)))
    (jlambda-methods-dispatcher method-finder)))


(define-syntax jlambda-methods-list
  (syntax-rules ()
    ((_ modifier class-object return-type method-name ((arg-type ...) ...))
     (list (extend-procedure (jlambda-method modifier class-object return-type method-name arg-type ...)
			     `(modifier ,class-object return-type method-name arg-type ...)) ...))))

(define-syntax jlambda-methods
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type method-name signatures)
	`(let* ((methods (jlambda-methods-list ,modifier ,class-object ,return-type ,method-name ,signatures))
		(method-finder (make-method-finder ,(->string (strip-syntax method-name)) methods)))
	   (extend-procedure (jlambda-methods-dispatcher method-finder)
			     (list ',modifier ,class-object ',return-type ,(->string method-name)))))))))


(define testo
  (jlambda-methods static (class java.lang.String) java.lang.String valueOf 
		   ((int) (long) (java.lang.Object))))
