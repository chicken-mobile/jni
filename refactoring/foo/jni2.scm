(import jni2-lolevel)

(include "jni-signatures.scm")
(include "jni-types.scm")
(include "jni-array.scm")
(include "jni-field-id.scm")
(include "jni-method-id.scm")
(include "jni-jvalues.scm")
(include "jni-jlambda-method.scm")
(include "jni-jlambda-field.scm")

(include "jni-reflection.scm")
(include "jni-exceptions.scm")
(include "jni-jlambda-methods-selection.scm")
(include "jni-jlambda-methods.scm")
(include "jni-jlambda.scm")

(module jni2
(object-hash-code)
(import chicken scheme foreigners jni2-lolevel lolevel)
(use lolevel)
(import jni-signatures jni-types jni-array jni-field-id jni-method-id
	jni-jvalues jni-exceptions jni-jlambda-method jni-jlambda-field
	jni-reflection jni-jlambda-methods jni-jlambda)

(define object-hash-code
  (jlambda java.lang.Object hashCode))

(mutate-procedure! ##sys#pointer->string
  (lambda (old)
    (lambda args
      (let ((arg (car args)))
	(if (eq? (jobject-ref-type arg) 0)
	    (apply old args)
	    (let* ((class-object (object-class arg))
		   (jobject-string (format "#<jref 0x~X [~A@~X]>" 
					   (pointer->address arg) (class-name class-object) (object-hash-code arg))))
	      (delete-local-ref class-object)
	      jobject-string))))))



(reexport jni2-lolevel
 jni-signatures jni-types jni-array jni-field-id jni-method-id
 jni-jvalues jni-exceptions jni-jlambda-method jni-jlambda-field
 jni-reflection jni-jlambda-methods-selection jni-jlambda-methods jni-jlambda))

