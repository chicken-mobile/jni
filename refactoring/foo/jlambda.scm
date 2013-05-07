(use jni-lolevel moremacros srfi-1)
(import-for-syntax jni-lolevel matchable moremacros srfi-1)
(jvm-init "../jni-utils.jar")

(include "signature.scm")
(include "class.scm")
(include "types.scm")
(include "array.scm")
(include "jlambda-method.scm")
(include "jlambda-field.scm")
(include "jlambda-methods.scm")
(include "exception.scm")



(define Class 
  (%class java.lang.Class))
(define Method
  (%class java.lang.reflect.Method))
(define Field
  (%class java.lang.reflect.Field))

(jlambda-method-define Class
  ()
  ((class-primitive? (boolean isPrimitive))
   (class-array?  (boolean isArray))
   (class-interface? (boolean isInterface))
   (class-local?     (boolean isLocalClass))
   (class-member?    (boolean isMemberClass))
   (class-synthetic? (boolean isSynthetic))

   (class-component-type        (java.lang.Class                  getComponentType))
   (class-modifiers             (int                              getModifiers))
   (class-package               (java.lang.Package                getPackage))
   (class-name                  (java.lang.String                 getName))
   (class-simple-name           (java.lang.String                 getSimpleName))
   (class-methods               (#(java.lang.reflect.Method)      getMethods))
   (class-declared-methods      (#(java.lang.reflect.Method)      getDeclaredMethods))
   (class-constructors          (#(java.lang.reflect.Constructor) getConstructors))
   (class-declared-constructors (#(java.lang.reflect.Constructor) getDeclaredConstructors))
   (class-constructor           (java.lang.reflect.Constructor    getConstructor         #(java.lang.Class)))
   (class-declared-constructor  (java.lang.reflect.Constructor    getDeclaredConstructor #(java.lang.Class)))
   (class-fields                (#(java.lang.reflect.Field)       getFields))
   (class-declared-fields       (#(java.lang.reflect.Field)       getDeclaredFields))))

(jlambda-method-define Method
  ()
  ((method-modifiers       (int                getModifiers))
   (method-return-type     (java.lang.Class    getReturnType))
   (method-name            (java.lang.String   getName))
   (method-parameter-types (#(java.lang.Class) getParameterTypes))
   (method-declaring-class (java.lang.Class    getDeclaringClass))
   (method-exception-types (#(java.lang.Class) getExceptionTypes))))

(jlambda-method-define Field
  ()
  ((field-modifiers       (int                getModifiers))
   (field-type            (java.lang.Class    getType))
   (field-name            (java.lang.String   getName))
   (field-declaring-class (java.lang.Class    getDeclaringClass))))


(define (find-method-by-name method-array name)
  (array-filter (lambda (x) (equal? (jstring->string! (method-name x)) name)) method-array))
(define (find-field-by-name field-array name)
  (array-filter (lambda (x) (equal? (jstring->string! (field-name x))  name)) field-array))

(define class-simple-name
  (compose jstring->string! class-simple-name))
(define class-name
  (compose jstring->string! class-name))
(define class-component-type
  (compose prepare-local-jobject class-component-type))
(define method-return-type
  (compose prepare-local-jobject method-return-type))

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


(define-syntax jlambda-method
  (syntax-rules ()
    ((_ modifier class-object return-type method-name ((arg-types ...)))
     (jlambda-non-overloaded-method modifier class-object return-type method-name arg-types ...))
    ((_ modifier class-object return-type method-name ((arg-types ...) ...))
     (jlambda-overloaded-method     modifier class-object return-type method-name ((arg-types ...) ...)))))

(define (method-modifier-symbol method)
  (or (and (static? (method-modifiers method)) 'static) 'nonstatic))

(define (method-match->jlamba-method method-match class-object)
  (let* ((first-method    (car method-match))
	 (modifier        (method-modifier-symbol first-method))
	 (return-type     (string->symbol (class-name (method-return-type first-method))))
	 (identifier      (string->symbol (jstring->string! (method-name first-method))))
	 (parameter-types-list (map method-parameter-types method-match))
	 (arg-types       (parameter-types->arg-types parameter-types-list)))
    (map delete-local-ref parameter-types-list)
    `(jlambda-method ,modifier ,class-object ,return-type ,identifier ,arg-types)))


(define-syntax jlambda
  (ir-macro-transformer
   (lambda (x i c)
     (match (strip-syntax x)
       ((_ class-name identifier)
	(let* ((class-object (find-class/error (mangle-class-name class-name)))
	       
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

	    (let ((result (if (null? matching-methods)
			      (error 'jlambda "field lookup not implemented")
			      `(let ((class-object (class ,class-name)))
				 ,(method-match->jlamba-method match 'class-object)))))


	      (delete-local-ref class-object)
	      (delete-local-ref all-methods)
	      (delete-local-ref all-fields)
	      (map delete-local-ref matching-methods)
	      (map delete-local-ref matching-fields)

	      result))))))))


(define (foo)
  (let loop ((i 0 )) 
    (let ((jstring-value ((jlambda java.lang.String valueOf) 123)))
      (print i " " jstring-value)
      (delete-local-ref jstring-value))
    (loop (+ i 1))))
