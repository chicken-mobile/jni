(use jni2)
(import-for-syntax chicken scheme extras matchable jni-jlambda-method jni-types jni-reflection jni2-lolevel)
(begin-for-syntax
 (require-library jni-types jni-jlambda-method jni-reflection jni2-lolevel))

(define (foof2 getter array)
  (array-fold (lambda (method foof2-list)
		(let ((key (string->symbol (to-string (getter method))))
		      (modifier (method-modifier-symbol method)))

		  (let* ((method-list (or (alist-ref key foof2-list) '()))
			 (name (string->symbol (to-string (getter method)))))

		    
		    (let ((static-methods-list (or (alist-ref 'static method-list) '()))
			  (foof (alist-update! 'static (cons method static-methods-list) foof2-list))))
		    
		    (let ())
		    )
#;
		  (alist-update! key
				 (alist-update! modifier (cons method (or (alist-ref key foof2-list) '())) foof2-list)

				 foof2-list equal?)))
	      '() array))


(define-syntax %jlambda-method
  (syntax-rules ()
    ((_ modifier class-object return-type method-name ((arg-types ...)))
     (jlambda-non-overloaded-method modifier class-object return-type method-name arg-types ...))
    ((_ modifier class-object return-type method-name ((arg-types ...) ...))
     (jlambda-overloaded-method     modifier class-object return-type method-name ((arg-types ...) ...)))))

(define (parameter-types->arg-types parameter-types-list)
  (reverse
   (map (lambda (parameter-types)
	  (array-map (lambda (type)
		       (let ((result
			      (if (class-array? type)
				  `#(,(string->symbol (class-name (class-component-type type))))
				  (string->symbol (class-name type)))))
			 (delete-local-ref type) result)) parameter-types)) 
	parameter-types-list)))

(define-for-syntax (%method-match->jlamba-method method-match class-object)
  (let* ((first-method    (car method-match))
	 (modifier        (method-modifier-symbol first-method))
	 (return-type     
	  (let ((type (method-return-type first-method)))
	    (if (class-array? type)
		`#(,(string->symbol (class-name (class-component-type type))))
		(string->symbol (class-name type)))))
	 (identifier      (string->symbol (to-string (method-name first-method))))
	 (parameter-types-list (map method-parameter-types method-match))
	 (arg-types       (parameter-types->arg-types parameter-types-list)))
    (map delete-local-ref parameter-types-list)
    `(define ,(symbol-append 'fooooo- identifier)
       (%jlambda-method ,modifier ,class-object ,return-type ,identifier ,arg-types))))

(define (field-match->jlamba-field field-match class-object)
  (let* ((first-field     (car field-match))
	 (modifier        (field-modifier-symbol first-field))
	 (identifier      (string->symbol (jstring->string (field-name first-field))))
	 (return-type     
	  (let ((type (field-type first-field)))
	    (if (class-array? type)
		`#(,(string->symbol (class-name (class-component-type type))))
		(string->symbol (class-name type))))))

    `(define ,(symbol-append 'fooooo- identifier)
       (jlambda-field ,modifier ,class-object ,return-type ,identifier))))

(define (unique-method-names methods)
  (unique-names methods method-name))


(define-syntax jimport
  (ir-macro-transformer
   (lambda (x i c)
     (match (strip-syntax x)
       ((_ class-name import-specifiers ...)
	(print class-name)
	(let* ((class-object (class* class-name))
	       (_ (print class-object))
	       (declared-fields  (class-declared-fields  class-object))
	       (declared-methods (class-declared-methods class-object)))

	  `(begin
	    
	    (use jni2 jni-jlambda)
            (import-for-syntax jni-jlambda)
            (begin-for-syntax
	     (require-library jni-jlambda))
	    
	    (define testo
	      (class java.lang.Integer))

	    ,@(map (cute %method-match->jlamba-method <> (i 'testo)) (map cdr (foof2 method-name declared-methods)))
#;	    ,@(map (cut field-match->jlamba-field   <> (i 'class)) (map cdr (foof2 field-name  declared-fields)))
)))))))
