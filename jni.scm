(module jni
()
(import chicken scheme extras)

(import-for-syntax jni-lolevel)
(begin-for-syntax
 (require-library jni-lolevel)
 (attach-thread))

(use lolevel jni-lolevel jni-types jni-jlambda-methods jni-reflection)
(attach-thread)

(define Object
  (class java.lang.Object))

(define object-hash-code
  (jlambda-method Object nonstatic int hashCode))

(mutate-procedure! ##sys#pointer->string
  (lambda (old)
    (lambda args
      (let* ((arg (car args))
	     (ref-type (jobject-ref-type arg)))
	(if (not (eq? ref-type 'local))
	    (apply old args)
	    (let* ((class-object (object-class arg))
		   (jobject-string (format "#<~A-jref 0x~X #<~A@~X>>"
					   ref-type (pointer->address arg) 
					   (class-name class-object) (object-hash-code arg))))
	      (delete-local-ref class-object)
	      jobject-string))))))


(reexport jni-lolevel 
	  jni-signatures jni-types jni-jvalues jni-array
	  jni-field-id jni-method-id
	  jni-jlambda-field jni-jlambda-method 
	  jni-jlambda-methods-selection jni-jlambda-methods
	  jni-reflection jni-jimport))
