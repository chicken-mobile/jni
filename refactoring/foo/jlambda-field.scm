(use jni-lolevel)
(import-for-syntax jni-lolevel matchable)
(include "field-id.scm")
(include "class.scm")

(define (make-getter-with-setter-variant modifier class-object field getter setter)
  (if (eq? modifier 'static) 
      (getter-with-setter (cut getter class-object field) (cut setter class-object field <>))
      (getter-with-setter (cut getter <> field)           (cut setter <> field <>))))

(define (make-field-getter-with-setter modifier class-object type field)
  (let ((static (eq? modifier 'static)))
    (make-getter-with-setter-variant modifier class-object field
     (case type
       ((boolean) (if static get-static-boolean-field get-boolean-field))
       ((byte)    (if static get-static-byte-field    get-byte-field))
       ((char)    (if static get-static-char-field    get-char-field))
       ((short)   (if static get-static-short-field   get-short-field))
       ((int)     (if static get-static-int-field     get-int-field))
       ((long)    (if static get-static-long-field    get-long-field))
       ((float)   (if static get-static-float-field   get-float-field))
       ((double)  (if static get-static-double-field  get-double-field))
       (else      (if static get-static-object-field  get-object-field)))
     (case type
       ((boolean) (if static set-static-boolean-field set-boolean-field))
       ((byte)    (if static set-static-byte-field    set-byte-field))
       ((char)    (if static set-static-char-field    set-char-field))
       ((short)   (if static set-static-short-field   set-short-field))
       ((int)     (if static set-static-int-field     set-int-field))
       ((long)    (if static set-static-long-field    set-long-field))
       ((float)   (if static set-static-float-field   set-float-field))
       ((double)  (if static set-static-double-field  set-double-field))
       (else      (if static set-static-object-field  set-object-field))))))

(define (jlambda-field* modifier class-object return-type field-name)
  (make-field-getter-with-setter modifier class-object return-type
    (field-id* modifier class-object return-type field-name)))



(define-for-syntax (%field-accessor-for action modifier type)
  (let ((sep (or (and (eq? modifier 'static) '-static-) '-)))
    (case type
      ((boolean byte char short int long float double)
       (symbol-append action sep type '-field))
      (else (symbol-append action sep 'object-field)))))

(define-for-syntax (%make-field-getter-with-setter modifier class-object return-type field)
  `(make-getter-with-setter-variant (quote ,modifier) ,class-object ,field
     ,(%field-accessor-for 'get modifier return-type)
     ,(%field-accessor-for 'set modifier return-type)))

(define-syntax jlambda-field
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type field-name)
	(let ((modifier    (strip-syntax modifier))
	      (return-type (strip-syntax return-type))
	      (%class-object (i 'class-object))
	      (%field        (i 'field)))

	  `(let ((,%class-object ,class-object))
	     (let ((,%field (%field-id ,modifier ,%class-object ,return-type ,field-name)))
	       ,(%make-field-getter-with-setter modifier %class-object return-type %field)))))))))

(define-for-syntax (field-spec modifier spec)
  (match spec
    ((proc-name (return-type field-name))
     `(,modifier ,return-type ,field-name ,proc-name))
    ((return-type field-name)
     `(,modifier ,return-type ,field-name ,field-name))))

(define-syntax jlambda-field-define*
  (syntax-rules ()
    ((_ class-object (modifier return-type field-name  proc-name) ...)
     (define-values (proc-name ...) 
       (values (jlambda-field modifier class-object return-type field-name) ...)))))

(define-syntax jlambda-field-define
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ class-object (static-fields ...) (nonstatic-fields ...))
	`(jlambda-field-define* ,class-object
				  ,@(map (cut field-spec 'static    <>)    static-fields)
				  ,@(map (cut field-spec 'nonstatic <>) nonstatic-fields)))))))
