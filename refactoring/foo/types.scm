(use jni-lolevel lolevel)

(define-record jobject-meta)
(define (jobject? pointer)
  (and (pointer? pointer)
       (jobject-meta? (pointer-tag pointer))))
(define (prepare-local-jobject jobject)
  (if (pointer? jobject)
      (set-finalizer! (tag-pointer jobject (make-jobject-meta)) delete-local-ref) jobject))

(mutate-procedure ##sys#pointer->string
  (lambda (old)
    (lambda args
      (let ((arg (car args)))
	(if (jobject-meta? (pointer-tag arg))
	    (let* ((object-class (object-class arg))
		   (jobject-string (format "#<jref <~A> ~A>" (to-string object-class) (to-string arg))))
	      (delete-local-ref object-class)
	      jobject-string)
	    (apply old args))))))
