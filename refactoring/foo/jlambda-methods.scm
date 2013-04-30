(use jni-lolevel lolevel)
(include "class.scm")
(include "jlambda-method.scm")

(define-syntax jlambda-methods-list
  (syntax-rules ()
    ((_ modifier class-object return-type method-name ((arg-type ...) ...))
     (list (extend-procedure (jlambda-method modifier class-object return-type method-name arg-type ...)
			     `(modifier ,class-object return-type method-name arg-type ...)) ...))))
