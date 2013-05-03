(use jni-lolevel moremacros)
(import-for-syntax jni-lolevel matchable moremacros)
(jvm-init)

(include "class.scm")
(include "types.scm")
(include "jlambda-method.scm")
(include "jlambda-field.scm")
(include "jlambda-methods.scm")

(define Class 
  (class java.lang.Class))
(define Method
  (class java.lang.reflect.Method))
(define Field
  (class java.lang.reflect.Method))

(define %call-object-method call-object-method)
(define call-object-method
  (lambda args (prepare-local-jobject (apply %call-object-method args))))

(define %call-static-object-method call-static-object-method)
(define call-static-object-method
  (lambda args (prepare-local-jobject (apply %call-static-object-method args))))


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
   (class-constructor           (#(java.lang.reflect.Constructor) getConstructor         #(java.lang.Class)))
   (class-declared-constructor  (#(java.lang.reflect.Constructor) getDeclaredConstructor #(java.lang.Class)))
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
   (field-return-type     (java.lang.Class    getReturnType))
   (field-name            (java.lang.String   getName))
   (field-parameter-types (#(java.lang.Class) getParameterTypes))
   (field-declaring-class (java.lang.Class    getDeclaringClass))
   (field-exception-types (#(java.lang.Class) getExceptionTypes))))

;; 1) field or method ?
;; 2) if method is it overloaded ?
;; 3) if not declared in class go to one with super class


;; (jlambda java.lang.String valueOf)
;; (jlambda java.lang.Integer valueOf)
;; (jlambda java.lang.Object toString)

(define (find-method-by-name method-list name)
  (filter (lambda (x) (equal? (jstring->string (method-name x)) name)) method-list))
(define (find-field-by-name field-list name)
  (filter (lambda (x) (equal? (jstring->string (field-name x)) name)) field-list))

(define %array->list array->list)
(define (array->list array)
  (map prepare-local-jobject (%array->list array)))

(define class-declared-methods
  (compose array->list class-declared-methods))
(define class-declared-fields
  (compose array->list class-declared-fields))
(define class-simple-name
  (compose jstring->string class-simple-name))
+(define class-name
  (compose jstring->string class-name))

(define method-parameter-types
  (compose array->list method-parameter-types))

(define (parameter-types->arg-types parameter-types-list)
  (map (lambda (parameter-types)
	 (map (lambda (type)
		(if (class-array? type)
		    (string->symbol (class-name (class-component-type type)))
		    (string->symbol (class-name type)))) parameter-types)) 
       parameter-types-list))

(define-syntax jlambda
  (ir-macro-transformer
   (lambda (x i c)
     (match (strip-syntax x)
       ((_ class-name identifier)
	(let* ((class-object (class* class-name))
	       
	       (all-methods (class-declared-methods class-object))
	       (all-fields  (class-declared-fields class-object))

	       (matching-methods (find-method-by-name all-methods (->string identifier)))
	       (matching-fields  (find-field-by-name  all-fields  (->string identifier))))
	  
	  (let ((match (or (and (not (null? matching-methods)) matching-methods)
			   (and (not (null? matching-fields))  matching-fields)
			   (error (format "no matching method/field found for ~A on ~A" identifier 
					  (jstring->string (class-simple-name class-object)))))))
	    (if (null? matching-methods)
		(error 'jlambda "field lookup not implemented")
		(let ((method match))
		  (if (> (length match) 1)
		      (let* ((parameter-types (map method-parameter-types method))
			     (arg-types (parameter-types->arg-types parameter-types)))
			(print arg-types)
			`(jlambda-methods static (class ,class-name)
					  java.lang.String ,identifier ,arg-types))
		      (let* ((method (car method))
			     (parameter-types (method-parameter-types method))
			     (modifier 'static)
			     (arg-types (map string->symbol (map class-simple-name parameter-types))))
			`(jlambda-method ,(strip-syntax modifier) (class ,class-name) 
					 java.lang.String ,identifier ,@arg-types))))) )))))))
