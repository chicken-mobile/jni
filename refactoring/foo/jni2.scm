(module jni2

(object-hash-code)
(import chicken scheme foreigners)
(use lolevel)

(import jni2-lolevel
 jni-signatures jni-types jni-array jni-field-id jni-method-id
 jni-jvalues jni-jlambda-method jni-jlambda-field
 jni-reflection jni-jlambda-methods jni-jlambda)

(define object-hash-code
  (jlambda java.lang.Object hashCode))

(mutate-procedure! ##sys#pointer->string
  (lambda (old)
    (lambda args
      (let* ((arg (car args))
	     (ref-type (jobject-ref-type arg)))
	(if (eq? ref-type 0)
	    (apply old args)
	    (let* ((class-object (object-class arg))
		   (jobject-string (format "#<~A-jref 0x~X #<~A@~X>>"
					   ref-type (pointer->address arg) (class-name class-object) (object-hash-code arg))))
	      (delete-local-ref class-object)
	      jobject-string))))))

(reexport jni2-lolevel
 jni-signatures jni-types jni-array jni-field-id jni-method-id
 jni-jvalues jni-jlambda-method jni-jlambda-field
 jni-reflection jni-jlambda-methods-selection jni-jlambda-methods jni-jlambda))

