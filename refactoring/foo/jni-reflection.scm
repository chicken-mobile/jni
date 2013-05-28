(module jni-reflection
*
(import chicken scheme data-structures)
(use jni2-lolevel jni-types jni-array jni-jlambda-method)

(define Class
  (class java.lang.Class))
(define Method
  (class java.lang.reflect.Method))
(define Field
  (class java.lang.reflect.Field))

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
  (array-filter (lambda (x) (equal? (jstring->string (method-name x)) name)) method-array))
(define (find-field-by-name field-array name)
  (array-filter (lambda (x) (equal? (jstring->string (field-name x))  name)) field-array))

(define class-simple-name
  (compose jstring->string class-simple-name))
(define class-name
  (compose jstring->string class-name))

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
)

