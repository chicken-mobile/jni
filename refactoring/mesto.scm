(use jni-lolevel matchable )
(import-for-syntax matchable chicken scheme extras)

(begin-for-syntax
 (require-library jni-lolevel)
 (jvm-init))

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


(define expand-type* expand-type)
(define-syntax expand-type
  (er-macro-transformer
   (lambda (x i c) (apply expand-type (cdr x)))))

(define mangle-class-name* mangle-class-name)
(define-syntax mangle-class-name
  (er-macro-transformer
   (lambda (x i c) (apply mangle-class-name (cdr x)))))


(define (find-class/error name)
  (let ((class-object (find-class name)))
    (if (or class-object (and (exception-check) (not (exception-clear))))
	(prepare-local-jobject class-object)
	(error 'find-class (format "no class named \"~A\" found :(" name)))))

(define (class* class-name)
  (find-class/error (or (and (symbol? class-name) (mangle-class-name* class-name)) class-name)))

(define-for-syntax (%find-class name safe?)
  (let* ((name (strip-syntax name))
	 (class-name (mangle-class-name name)))
    `(,(or (and safe? 'find-class) 'find-class/error) ,class-name)))

(define-syntax %class
  (ir-macro-transformer
   (lambda (x i c) (%find-class (cadr x) #t))))
(define-syntax class
  (ir-macro-transformer
   (lambda (x i c) (%find-class (cadr x) #f))))


(use expand-full)

(import foo)
(import-for-syntax foo)

;;
;; field-id's
;;

(define (get-field-id/error* variant args)
  (or (or (apply variant args) (and (exception-check) (not (exception-clear))))
      (match args ((class-object field-name type)
        (error (format "~A with type \"~A\" not found for ~A :(" 
		       field-name type (to-string class-object)))))))

(define (get-field-id/error #!rest args)
  (get-field-id/error* get-field-id args))
(define (get-static-field-id/error #!rest args)
  (get-field-id/error* get-static-field-id args))


(define (field-id-variant modifier #!optional (safe? #f))
  (print modifier)
  (if safe? 
      (if (eq? 'static modifier) get-static-field-id get-field-id)
      (if (eq? 'static modifier) get-static-field-id/error get-field-id/error)))

(define (field-id* modifier class-object return-type field-name)
  ((field-id-variant modifier) class-object field-name (expand-type* return-type)))

(define-for-syntax (%field-id-variant modifier #!optional (safe? #f))
  (print modifier)
  (if safe? 
      (if (eq? 'static modifier) 'get-static-field-id 'get-field-id)
      (if (eq? 'static modifier) 'get-static-field-id/error 'get-field-id/error)))

(define-for-syntax (%field-id* spec safe?)
  (match (strip-syntax spec)
    ((_ modifier class-object return-type field-name)
     (let ((name  (->string (strip-syntax field-name))))
       `(,(%field-id-variant modifier) ,class-object ,name ,(expand-type return-type))))))

(define-syntax %field-id
  (er-macro-transformer
   (lambda (x i c) (%field-id* x #t))))
(define-syntax field-id
  (er-macro-transformer
   (lambda (x i c) (%field-id* x #f))))



(define-for-syntax (field-accessors-for modifier type)
  (case type
    ((boolean byte char short int long float double)
     (if (eq? modifier 'static)
	 (symbol-append 'get-static type '-field)
	 (symbol-append 'get type '-field)))
    (else 
     (if (eq? modifier 'static)
	 get-static-object-field get-object-field))))

(define-for-syntax (make-field-getter modifier return-type class-object field)
  `())

(define-syntax jlambda-field
  (er-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type class-name field-name)
	`(let ((field (field-id ,modifier ,class-object ,return-type)))
	   (getter-with-setter 
	    (make-field-getter)
	    (make-field-setter))))))))


(define-for-syntax (%method-id-variant modifier)
  (or (and (eq? modifier 'static) 'get-static-method-id) 'get-method-id))
(define-for-syntax (method-id-variant modifier safe?)
  (let ((variant (%method-id-variant modifier)))
    (or (and safe? variant) (symbol-append variant '/error))))

(define-for-syntax (%method-id* spec safe?)
  (match (strip-syntax spec)
    ((_ modifier class-object return-type method-name arg-types ...)
     (let ((variant (method-id-variant (strip-syntax modifier) safe?))
	   (name    (symbol->string (strip-syntax method-name))))
       `(,variant ,class-object ,name (type-signature ,arg-types ,return-type))))))

(define (get-method-id/error* variant args)
  (or (or (apply variant args) (and (exception-check) (not (exception-clear))))
      (match args ((class-object method-name signature)
        (error (format "~A~A not found for ~A" method-name signature (to-string class-object)))))))

(define (get-method-id/error #!rest args)
  (get-method-id/error* get-method-id args))
(define (get-static-method-id/error #!rest args)
  (get-method-id/error* get-static-method-id args))


(define-syntax %method-id
  (ir-macro-transformer
   (lambda (x i c) (%method-id* x #t))))
(define-syntax method-id
  (ir-macro-transformer
   (lambda (x i c) (%method-id* x #f))))
(define (method-id* modifier class-object return-type method-name . arg-types)
  ((or (and (eq? modifier 'static) get-static-method-id/error) get-method-id/error)
   class-object method-name (expand-type arg-types return-type)))


(define (jlambda-method modifier class-object return-type method-name . args)
  (let ((method (method-id* modifier class-object return-type method-name args)))))
