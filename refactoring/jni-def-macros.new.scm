(use expand-full)
(import-for-syntax matchable)

(define-for-syntax (type->jtype stype)
  (let ((type-string (symbol->string stype)))
    (string-set! type-string 0 (char-upcase (string-ref type-string 0)))
    (string->symbol type-string)))

(define-for-syntax (call-procedure-foreign-name modifier type)
  (symbol-append 
   (or (and (eq? modifier 'nonstatic)
	    'Call) 'CallStatic) type 'MethodA))

(define-syntax export-type-procedure
  (ir-macro-transformer
   (lambda (x i c)
     (match (i x)
       ((_ return-type (procedure-name procedure-foreign-name) arg-types ...)
	`(begin
	   (export ,procedure-name)
	   (define ,procedure-name
	     (jni-env-lambda ,return-type ,procedure-foreign-name ,@arg-types))))))))




(define-for-syntax (procedure-name-for variant return-type)
  (symbol-append 
   (or (and (eq? modifier 'nonstatic)
	    'call-) 'call-static-) type '-method))




(define-syntax export-type-procedures-for
  (ir-macro-transformer
   (lambda (x i c)
     (match x ((_ variant type has-modifiers?)
	       (let ((name         (procedure-name-for variant 'int type))
		     (foreign-name (procedure-foreign-name-for variant 'int type)))

		 `(,name ,foreign-name)))))))

(pp (expand* '(export-type-procedures-for call method)))
(exit -1)

(export-type-procedures-for call method #t jobject jmethod-id jvalue)
(export-type-procedures-for get  field  #t jobject  jfield-id)
(export-type-procedures-for set  field  #t jobject  jfield-id jvalue)
(export-type-procedures-for set  jvalue #f jvalue )



(define-syntax export-call-procedures-for
  (ir-macro-transformer
   (lambda (x i c)
     (let ((type (strip-syntax (cadr x))))
       (let ((static-call-procedure-name 
	      (call-procedure-name            'static type))
	     (static-call-procedure-foreign-name 
	      (call-procedure-foreign-name    'static (type->jtype type)))
	     (nonstatic-call-procedure-name 
	      (call-procedure-name         'nonstatic type))
	     (nonstatic-call-procedure-foreign-name 
	      (call-procedure-foreign-name 'nonstatic (type->jtype type))))
	 `(begin
	    (export-type-procedure ,type 
	      (,static-call-procedure-name ,static-call-procedure-foreign-name) jobject jmethod-id jvalue)
	    (export-type-procedure ,type 
	      (,nonstatic-call-procedure-name ,nonstatic-call-procedure-foreign-name) jobject jmethod-id jvalue)))))))

(define-for-syntax (field-procedure-name modifier type variant)
  (symbol-append 
   (or (and (eq? variant 'get)
	    (or (and (eq? modifier 'nonstatic) 'get-) 'get-static-))
       (or (and (eq? modifier 'nonstatic) 'set-) 'set-static-))  type '-field))
(define-for-syntax (field-procedure-foreign-name modifier variant type)
  (symbol-append 
   (or (and (eq? variant 'Get)
	    (or (and (eq? modifier 'nonstatic) 'Get) 'GetStatic))
       (or (and (eq? modifier 'nonstatic) 'Set) 'SetStatic))  type 'Field))

(define-syntax export-field-procedures-for
  (ir-macro-transformer
   (lambda (x i c)
     (let ((type (strip-syntax (cadr x))))
       (let ((static-field-procedure-getter-name 
	      (field-procedure-name 'static type 'get))
	     (static-field-procedure-getter-foreign-name 
	      (field-procedure-foreign-name 'static (type->jtype type) 'get))
	     (nonstatic-field-procedure-getter-name 
	      (field-procedure-name 'nonstatic type 'get))
	     (nonstatic-field-procedure-getter-name 
	      (field-procedure-foreign-name 'nonstatic (type->jtype type) 'get)))
	 `(begin
	    (export-type-procedure ,type 
	      (,static-call-procedure-name ,static-call-procedure-foreign-name) jobject jmethod-id jvalue)
	    (export-type-procedure ,type 
	      (,nonstatic-call-procedure-name ,nonstatic-call-procedure-foreign-name) jobject jmethod-id jvalue)))))))
