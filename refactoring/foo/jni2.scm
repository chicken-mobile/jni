(module jni2

(object-hash-code)
(import chicken scheme extras foreigners)

(use lolevel format jni2-lolevel
     jni-signatures jni-types jni-array jni-field-id jni-method-id
     jni-jvalues jni-jlambda-method jni-jlambda-field
     jni-reflection jni-jlambda-methods jni-jlambda)

(define object-hash-code
  (jlambda java.lang.Object hashCode))

(mutate-procedure! ##sys#pointer->string
  (lambda (old)
    (lambda args
      (let* ((arg (car args))
	     (ref-type (jobject-ref-type arg))
	     (class-object (object-class arg)))
	(if (or (eq? ref-type 'invalid) (and (not class-object) (exception-occurred))) ;; jmethod/jfield-id is a weak reference ?!?
	    (apply old args)
	    (let ((jobject-string (format "#<~A-jref 0x~X #<~A@~X \"~A\">>"
					  ref-type (pointer->address arg) 
					  (class-name class-object) (object-hash-code arg) (to-string arg))))
	      (delete-local-ref class-object)
	      jobject-string))))))

(reexport jni2-lolevel
 jni-signatures jni-types jni-array jni-field-id jni-method-id
 jni-jvalues jni-jlambda-method jni-jlambda-field
 jni-reflection jni-jlambda-methods-selection jni-jlambda-methods jni-jlambda))
