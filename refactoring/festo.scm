(import chicken scheme foreign lolevel data-structures jni-lolevel)
(import-for-syntax chicken scheme extras matchable jni-lolevel symbol-utils)
(begin-for-syntax
 (require-library jni-lolevel)
 (jvm-init))

;;
;; Classes
;;

(define (find-class/error name)
  (let ((class-object (find-class name)))
    (if (or class-object (and (exception-check) (not (exception-clear))))
	(prepare-local-jobject class-object)
	(error 'find-class (format "no class named \"~A\" found :(" name)))))

(define (class* class-name)
  (find-class/error (or (and (symbol? class-name) (mangle-class-name class-name)) class-name)))

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


;;
;; method-id's
;;

(define (get-method-id/error* variant args)
  (or (or (apply variant args) (and (exception-check) (not (exception-clear))))
      (match args ((class-object method-name signature)
        (error (format "~A~A not found for ~A" method-name signature (to-string class-object)))))))

(define (get-method-id/error #!rest args)
  (get-method-id/error* get-method-id args))
(define (get-static-method-id/error #!rest args)
  (get-method-id/error* get-static-method-id args))

(define (method-id* modifier class-object return-type method-name . arg-types)
  ((or (and (eq? modifier 'static) get-static-method-id/error) get-method-id/error)
   class-object method-name (expand-type arg-types return-type)))


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

(define-syntax %method-id
  (ir-macro-transformer
   (lambda (x i c) (%method-id* x #t))))
(define-syntax method-id
  (ir-macro-transformer
   (lambda (x i c) (%method-id* x #f))))


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

(define (field-id* modifier class-object return-type field-name)
  ((or (and (eq? modifier 'static) get-static-field-id/error) get-field-id/error)
   class-object (->string field-name) (expand-type return-type)))


(define-for-syntax (%field-id-variant modifier)
  (or (and (eq? modifier 'static) 'get-static-field-id) 'get-field-id))
(define-for-syntax (field-id-variant modifier safe?)
  (let ((variant (%field-id-variant modifier)))
    (or (and safe? variant) (symbol-append variant '/error))))

(define-for-syntax (%field-id* spec safe?)
  (match (strip-syntax spec)
    ((_ modifier class-object return-type field-name)
     (let ((variant (field-id-variant (strip-syntax modifier) safe?))
	   (name    (symbol->string (strip-syntax field-name))))
       `(,variant ,class-object ,name (type-signature ,return-type))))))

(define-syntax %field-id
  (ir-macro-transformer
   (lambda (x i c) (%field-id* x #t))))
(define-syntax field-id
  (ir-macro-transformer
   (lambda (x i c) (%field-id* x #f))))

;;;;
;;;;;;
;; jlambda
;;;;;;
;;;;

;;
;; method
;;

(define-for-syntax (call-proc-variant modifier return-type)
  (symbol-append
   (if (eq? 'nonstatic modifier)
       'call- 'call-static-) (type->native-type return-type) '-method))

(define-for-syntax (jlambda-postcall return-type call-variant)
  (case return-type
    ((boolean byte char short int long float double) call-variant)
    (else `(prepare-local-jobject ,call-variant))))

(define-for-syntax (arg-types->arg-names* arg-types)
  (map (lambda (type)
	 (cond ((symbol? type) (symbol-append type))
	       ((vector? type) (symbol-append (vector-ref type 0) 'Array))
	       (else `(error "Invalid Java type signature" ,type))))
       arg-types))

(define-for-syntax (jlambda-args modifier arg-names)
  (or (and (eq? modifier 'static) arg-names) (cons 'target arg-names)))

(define-syntax jlambda-call
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier target return-type method jvalue-array)
	(let ((%variant (call-proc-variant (strip-syntax modifier) (strip-syntax return-type))))
	  (jlambda-postcall (strip-syntax return-type) 
			    `(,%variant target ,method jvalue-array))))))))

(define-syntax jlambda-method
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type method-name arg-types ...)
	(let ((arg-names (arg-types->arg-names* arg-types)))
	  `(let ((method (method-id ,modifier ,class-object ,return-type ,method-name ,@arg-types))
		 (jvalue-array (make-jvalue-array ,(length arg-types)))
		 (target class-object))
	     (lambda ,(jlambda-args (i modifier) arg-names)
	       (fill-jvalue-array jvalue-array ,arg-types ,arg-names)
	       (jlambda-call ,modifier target ,return-type method jvalue-array)))))))))
