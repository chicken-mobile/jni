(use jni-lolevel matchable)
(import-for-syntax chicken jni-lolevel matchable)
(include "class.scm")


(define (get-method-id/error* variant args)
  (or (or (apply variant args) (and (exception-check) (not (exception-clear))))
      (match args ((class-object method-name signature)
        (error (format "~A~A not found for ~A :(" 
		       method-name signature (to-string class-object)))))))

(define (get-method-id/error #!rest args)
  (get-method-id/error* get-method-id args))
(define (get-static-method-id/error #!rest args)
  (get-method-id/error* get-static-method-id args))


(define (method-id-variant modifier #!optional (safe? #f))
  (or (and (eq? modifier 'static) 
	   (or (and safe? get-static-method-id) get-static-method-id/error))
      (or (and safe? get-method-id) get-method-id/error)))

(define (method-id* modifier class-object return-type method-name arg-types)
  ((method-id-variant modifier) class-object method-name (expand-type arg-types return-type)))


(define-for-syntax (%method-id-variant modifier #!optional (safe? #f))
  (or (and (eq? modifier 'static) 
	   (or (and safe? 'get-static-method-id) 'get-static-method-id/error))
      (or (and safe? 'get-method-id) 'get-method-id/error)))

(define-for-syntax (%method-id* spec safe?)
  (match (strip-syntax spec)
    ((_ modifier class-object return-type method-name arg-types ...)
     (let ((name  (->string (strip-syntax method-name))))
       `(,(%method-id-variant modifier safe?) ,class-object ,name (type-signature ,arg-types ,return-type))))))

(define-syntax %method-id
  (ir-macro-transformer
   (lambda (x i c) (%method-id* x #t))))
(define-syntax method-id
  (ir-macro-transformer
   (lambda (x i c) (%method-id* x #f))))

