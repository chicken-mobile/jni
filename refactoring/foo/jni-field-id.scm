(module jni-field-id
*
(import chicken scheme extras matchable)
(use jni2-lolevel jni-signatures)
(import-for-syntax chicken matchable jni-signatures)
(begin-for-syntax
 (require-library jni-signatures))

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
  (or (and (eq? modifier 'static) 
	   (or (and safe? get-static-field-id) get-static-field-id/error))
      (or (and safe? get-field-id) get-field-id/error)))

(define (field-id* modifier class-object return-type field-name)
  ((field-id-variant modifier) class-object field-name (expand-type return-type)))


(define-for-syntax (%field-id-variant modifier #!optional (safe? #f))
  (or (and (eq? modifier 'static) 
	   (or (and safe? 'get-static-field-id) 'get-static-field-id/error))
      (or (and safe? 'get-field-id) 'get-field-id/error)))

(define-for-syntax (%field-id* spec safe?)
  (match (strip-syntax spec)
    ((_ modifier class-object return-type field-name)
     (let ((name  (->string (strip-syntax field-name))))
       `(,(%field-id-variant modifier safe?) ,class-object ,name ,(expand-type return-type))))))

(define-syntax %field-id
  (ir-macro-transformer
   (lambda (x i c) (%field-id* x #t))))
(define-syntax field-id
  (ir-macro-transformer
   (lambda (x i c) (%field-id* x #f))))
)
