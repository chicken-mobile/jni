(module jni-jlambda
*
(import chicken scheme matchable extras data-structures)
(import-for-syntax matchable jni2-lolevel jni-signatures jni-types jni-array jni-reflection)
(begin-for-syntax
 (require-library jni2-lolevel jni-signatures jni-types jni-array jni-reflection))

(define-for-syntax (parameter-types->arg-types parameter-types-list)
  (reverse
   (map (lambda (parameter-types)
	  (array-map (lambda (type)
		       (let ((result
			      (if (class-array? type)
				  `#(,(string->symbol (class-name (class-component-type type))))
				  (string->symbol (class-name type)))))
			 (delete-local-ref type) result)) parameter-types)) 
	parameter-types-list)))

(define-for-syntax (method-modifier-symbol method)
  (or (and (static? (method-modifiers method)) 'static) 'nonstatic))
(define-for-syntax (field-modifier-symbol method)
  (or (and (static? (field-modifiers method)) 'static) 'nonstatic))

(define-for-syntax (method-match->jlamba-method method-match class-object)
  (let* ((first-method    (car method-match))
	 (modifier        (method-modifier-symbol first-method))
	 (return-type     (string->symbol (class-name (method-return-type first-method))))
	 (identifier      (string->symbol (jstring->string (method-name first-method))))
	 (parameter-types-list (map method-parameter-types method-match))
	 (arg-types       (parameter-types->arg-types parameter-types-list)))
    (map delete-local-ref parameter-types-list)
    `(jlambda-method ,modifier ,class-object ,return-type ,identifier ,arg-types)))

(define-for-syntax (field-match->jlamba-field field-match class-object)
  (let* ((first-field     (car field-match))
	 (modifier        (field-modifier-symbol first-field))
	 (return-type     (string->symbol (class-name (field-type first-field))))
	 (identifier      (string->symbol (jstring->string (field-name first-field)))))
    `(jlambda-field ,modifier ,class-object ,return-type ,identifier)))


(define-syntax jlambda-method
  (syntax-rules ()
    ((_ modifier class-object return-type method-name ((arg-types ...)))
     (jlambda-non-overloaded-method modifier class-object return-type method-name arg-types ...))
    ((_ modifier class-object return-type method-name ((arg-types ...) ...))
     (jlambda-overloaded-method     modifier class-object return-type method-name ((arg-types ...) ...)))))


(define-syntax jlambda
  (er-macro-transformer
   (lambda (x i c)
     (match (strip-syntax x)
       ((_ class-name identifier)
	(let* ((class-object (class* (mangle-class-name class-name)))
	       
	       (all-methods (class-declared-methods class-object))
	       (all-fields  (class-declared-fields class-object))

	       (matching-methods (find-method-by-name all-methods (->string identifier)))
	       (matching-fields  (find-field-by-name  all-fields  (->string identifier))))

	  (let ((match (or (and (not (null? matching-methods)) matching-methods)
			   (and (not (null? matching-fields))  matching-fields)

			   (begin
			     (delete-local-ref class-object)
			     (delete-local-ref all-methods)
			     (delete-local-ref all-fields)
			     (map delete-local-ref matching-methods)
			     (map delete-local-ref matching-fields)

			     (let ((class-name (class-simple-name class-object)))
			       (delete-local-ref class-object)
			       (exception-clear)
			       (error (format "no matching method/field found for ~A on ~A" identifier class-name)))))))

	    (let ((result `(let ((,(i class-name) (class ,class-name)))
			     ,(if (null? matching-methods)
				  (field-match->jlamba-field match (i class-name))
				  (method-match->jlamba-method match (i class-name))))))

	      (delete-local-ref class-object)
	      (delete-local-ref all-methods)
	      (delete-local-ref all-fields)
	      (map delete-local-ref matching-methods)
	      (map delete-local-ref matching-fields)

	      result))))))))
)
