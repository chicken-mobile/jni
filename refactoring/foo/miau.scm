(import-for-syntax matchable)
(use jni)
(attach-thread)

(define-syntax define-array-procedure
  (er-macro-transformer
   (lambda (x r c)
     (match x
       ((_ type return-type proc-format func-format args ...)
	(let ((proc-name (string->symbol (format proc-format (->string type))))
	      (func-name (string->symbol (format func-format (string-upcase! (->string type) 0 1)))))
	  `(define ,proc-name
	     (jni-env-lambda ,return-type ,func-name ,@args))))))))

(define-syntax define-array-procedures
  (syntax-rules ()
    ((_ type ...)
     (begin
       (define-array-procedure type jarray "new-~A-array" "New~AArray" size_t) ...
       (define-array-procedure type (c-pointer type) "array-~As" "Get~AArrayElements" jarray size_t size_t pointer-vector) ...
       (define-array-procedure type (c-pointer type) "array-~As-at" "Get~AArrayRegion" jarray size_t size_t pointer-vector) ...
       (define-array-procedure type void "set-array-~As-at!" "Set~AArrayRegion" jarray size_t size_t pointer-vector) ...
       (define-array-procedure type void "release-array-~As" "Release~AArrayElements" pointer-vector bool) ...))))


(define (delete-local-ref* jobject)
  (delete-local-ref jobject))

(define (array-map! class procedure array #!optional (start 0) (end #f))
  (let* ((end (or end (array-length array)))
	 (elements-count (- end start))
	 (mapped-array (make-array elements-count class #f) ))

    (let ((result (let loop ((idx start))
		    (or (and (= idx end) mapped-array)
			(let ((array-element (array-ref array idx)))
			  (array-set! mapped-array idx (procedure array-element))
			  (delete-local-ref* array-element)
			  (loop (+ idx 1)))))))
      (delete-local-ref* array) result)))


(define (array-copy* source-array destination-array start end)
  (let loop ((source-idx start)
	     (destination-idx 0))
    (if (= source-idx (+ 1 end))
	destination-array
	(let ((source-element (array-ref* source-array source-idx)))
	  (array-set!* destination-array destination-idx source-element)
	  (delete-local-ref* source-element)
	  (loop (+ source-idx 1)
		(+ destination-idx 1))))))

(define (jexception-check jobject)
  (assert (or jobject (not (exception-check)))
	  (let* ((occurred-exception (exception-occurred))
		 (exception-name (class-name (object-class occurred-exception))))
	    (delete-local-ref occurred-exception) (exception-clear)     
	    (format "Exception Occurred :(( \n\n~A" exception-name))) jobject)

(define (array-ref* array idx)
  (jexception-check (array-ref array idx)))
(define (array-set!* array idx value)
  (jexception-check (array-set! array idx value)))




(define (array-copy source-array start end #!optional (initial-value #f))
  (let ((destination-array 
	 (make-array (- (- 1 end) start) (class-component-type (object-class source-array)) initial-value)))
    (array-copy* source-array destination-array start end)))

(define (array-copy! source-array start end #!optional (initial-value #f))
  (let ((result (array-copy source-array start end initial-value)))
    (delete-local-ref* source-array) result))


(define (array-move source-array destination-array source-idx destination-idx copy-length)
  (let loop ((idx 0))
    (or (and (= idx copy-length) destination-array)
	(let ((source-element (array-ref* source-array (+ source-idx idx))))
	  (array-set!* destination-array (+ destination-idx idx) source-element)
	  (delete-local-ref source-element)
	  (loop (+ 1 idx))))))

(define (%array-filter predicate array)
  (let* ((array-class-object (object-class array))
	 (class-object (class-component-type array-class-object))
	 (buffer-length (array-length array))
	 (buffer (make-array buffer-length class-object #f)))
    (delete-local-ref* array-class-object)
    (let ((result-length
	   (array-fold
	    (lambda (array-element current-length)
	      (let* ((result (predicate array-element))
		     (new-length (or (and result (+ 1 current-length)) current-length)))
		(unless (not result)
		  (array-set!* buffer current-length array-element))
		(delete-local-ref* array-element) new-length)) 0 array)))

      (let ((result-array (make-array result-length class-object #f)))
	(delete-local-ref* class-object)	
	(array-move buffer result-array 0 0 result-length)
	(delete-local-ref* buffer) result-array))))

(define (%%array-filter predicate array)
  (push-local-frame 3)
  (let* ((class-object (class-component-type (object-class array)))
	 (buffer-length (array-length array))
	 (buffer (make-array buffer-length class-object #f)))

    (let ((result-length
	   (array-fold
	    (lambda (array-element current-length)
	      (let ((result (predicate array-element)))
		(if result (array-set!* buffer current-length array-element))
		(delete-local-ref* array-element)
		(or (and result (+ 1 current-length)) current-length))) 0 array)))

      (let ((result-array (make-array result-length class-object #f)))
	(array-move buffer result-array 0 0 result-length)
	(pop-local-frame result-array)))))

(define (%array-filter! predicate array)
  (let ((result (array-filter predicate array)))
    (delete-local-ref* array) result))


(define-syntax %count
  (er-macro-transformer
   (lambda (x r c) (length (cdr x)))))

(define-syntax with-refs*
  (syntax-rules ()
    ((_ ((ref make-ref) ...) body ... last-body)
     (let* ((ref make-ref) ...)
       (push-local-frame (%count ref ...))
       body ...
       (let ((result last-body))
	 (if (and (pointer? result) (eq? (jobject-ref-type result) 'local))
	     (pop-local-frame result) 
	     (begin (pop-local-frame #f) result)))))))

(define-syntax %with-refs*
  (syntax-rules ()
    ((_ ((ref make-ref) ...) body ... last-body)
     (let* ((ref make-ref) ...)
       (push-local-frame (%count ref ...)) 
       body ... 
       (pop-local-frame last-body)))))

;; up to 3 times slower :(
(define (array-filter predicate array)
  (%with-refs* 
    ((array-class   (object-class array))
     (class-object  (class-component-type array-class))
     (buffer-length (array-length array))
     (buffer        (make-array buffer-length class-object #f)))
   
   (let ((result-length
	  (array-fold
	   (lambda (array-element current-length)
	     (let ((result (predicate array-element)))
	       (if result (array-set!* buffer current-length array-element))
	       (delete-local-ref* array-element)
	       (or (and result (+ 1 current-length)) current-length))) 0 array)))
     (array-move buffer (make-array result-length class-object #f) 0 0 result-length))))

(define (array-filter! predicate array)
  (let ((result (%array-filter predicate array)))
    (delete-local-ref* array) result))


(define testo 
  (jlambda-method (class java.lang.reflect.Method) nonstatic boolean isVarArgs))

(define (array-filter-test n)
  (print "%array-filter")
  (time 
   (let loop ((i 0))
     (let* ((object-class (class java.lang.Class))
	    (declared-methods (class-declared-methods object-class))
	    (result (%array-filter testo declared-methods)))         
       (delete-local-ref object-class)
       (delete-local-ref declared-methods)
       (delete-local-ref result)

       (unless (= i n) (loop (+ 1 i))))))

  (print "%%array-filter")
  (time 
   (let loop ((i 0))
     (let* ((object-class (class java.lang.Class))
	    (declared-methods (class-declared-methods object-class))
	    (result (%%array-filter testo declared-methods)))         
       (delete-local-ref object-class)
       (delete-local-ref declared-methods)
       (delete-local-ref result)

       (unless (= i n) (loop (+ 1 i))))))

  (print "array-filter")
  (time 
   (let loop ((i 0))
     (let* ((object-class (class java.lang.Class))
	    (declared-methods (class-declared-methods object-class))
	    (result (array-filter testo declared-methods)))         
       (delete-local-ref object-class)
       (delete-local-ref declared-methods)
       (delete-local-ref result)

       (unless (= i n) (loop (+ 1 i)))))))
