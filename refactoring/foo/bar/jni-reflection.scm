(module jni-reflection
(Class
 class-primitive? class-array? class-interface? class-local? class-member? class-synthetic?
 class-superclass class-interfaces class-classes class-component-type
 class-modifiers class-name class-simple-name class-package
 class-method class-methods class-declared-method class-declared-methods 
 class-field class-fields class-declared-field class-declared-fields
 class-constructor class-constructors class-declared-constructor class-declared-constructors

 Constructor
 constructor-modifiers constructor-parameter-types
 constructor-declaring-class constructor-exception-types

 Method
 method-modifiers method-name method-declaring-class
 method-return-type method-parameter-types method-exception-types

 Field
 field-modifiers field-name field-declaring-class field-type)

(import chicken scheme matchable)
(use matchable data-structures jni-lolevel jni-types jni-array jni-jlambda-methods)
(attach-thread)

(define Class
  (class java.lang.Class))
(define Constructor
  (class java.lang.reflect.Constructor))
(define Method
  (class java.lang.reflect.Method))
(define Field
  (class java.lang.reflect.Field))

(define-jlambda-methods Class
  (class-primitive? (nonstatic boolean isPrimitive))
  (class-array?     (nonstatic boolean isArray))
  (class-interface? (nonstatic boolean isInterface))
  (class-local?     (nonstatic boolean isLocalClass))
  (class-member?    (nonstatic boolean isMemberClass))
  (class-synthetic? (nonstatic boolean isSynthetic))

  (class-superclass            (nonstatic java.lang.Class                  getSuperclass))
  (class-interfaces            (nonstatic #(java.lang.Class)               getInterfaces))
  (class-classes               (nonstatic #(java.lang.Class)               getClasses))
  (class-component-type        (nonstatic java.lang.Class                  getComponentType))
  (class-modifiers             (nonstatic int                              getModifiers))
  (class-package               (nonstatic java.lang.Package                getPackage))
  (class-name                  (nonstatic java.lang.String                 getName))
  (class-simple-name           (nonstatic java.lang.String                 getSimpleName))
  (class-method                (nonstatic java.lang.reflect.Method         getMethod java.lang.String #(java.lang.Class)))
  (class-declared-method       (nonstatic java.lang.reflect.Method         getDeclaredMethod java.lang.String #(java.lang.Class)))
  (class-methods               (nonstatic #(java.lang.reflect.Method)      getMethods))
  (class-declared-methods      (nonstatic #(java.lang.reflect.Method)      getDeclaredMethods))
  (class-constructors          (nonstatic #(java.lang.reflect.Constructor) getConstructors))
  (class-declared-constructors (nonstatic #(java.lang.reflect.Constructor) getDeclaredConstructors))
  (class-constructor           (nonstatic java.lang.reflect.Constructor    getConstructor         #(java.lang.Class)))
  (class-declared-constructor  (nonstatic java.lang.reflect.Constructor    getDeclaredConstructor #(java.lang.Class)))
  (class-field                 (nonstatic java.lang.reflect.Field          getField java.lang.String))
  (class-declared-field        (nonstatic java.lang.reflect.Field          getDeclaredField java.lang.String))
  (class-fields                (nonstatic #(java.lang.reflect.Field)       getFields))
  (class-declared-fields       (nonstatic #(java.lang.reflect.Field)       getDeclaredFields)))

(define-jlambda-methods Constructor 
  (constructor-modifiers       (nonstatic int                getModifiers))
  (constructor-parameter-types (nonstatic #(java.lang.Class) getParameterTypes))
  (constructor-declaring-class (nonstatic java.lang.Class    getDeclaringClass))
  (constructor-exception-types (nonstatic #(java.lang.Class) getExceptionTypes)))

(define-jlambda-methods Method  
  (method-modifiers       (nonstatic int                getModifiers))
  (method-return-type     (nonstatic java.lang.Class    getReturnType))
  (method-name            (nonstatic java.lang.String   getName))
  (method-parameter-types (nonstatic #(java.lang.Class) getParameterTypes))
  (method-declaring-class (nonstatic java.lang.Class    getDeclaringClass))
  (method-exception-types (nonstatic #(java.lang.Class) getExceptionTypes)))

(define-jlambda-methods Field
  (field-modifiers       (nonstatic int                getModifiers))
  (field-type            (nonstatic java.lang.Class    getType))
  (field-name            (nonstatic java.lang.String   getName))
  (field-declaring-class (nonstatic java.lang.Class    getDeclaringClass)))

(define (find-method-by-name method-array name)
  (array-filter (lambda (x) (equal? (jstring->string (method-name x)) name)) method-array))
(define (find-field-by-name field-array name)
  (array-filter (lambda (x) (equal? (jstring->string (field-name x))  name)) field-array))

(define class-simple-name
  (compose jstring->string class-simple-name))
(define class-name
  (compose string->symbol jstring->string class-name))

(define method-name
  (compose string->symbol jstring->string method-name))

(define field-name
  (compose string->symbol jstring->string field-name))

(define (parameter-types->arg-types parameter-types-list)
  (reverse
   (map (lambda (parameter-types)
	  (array-map (lambda (type)
		       (let ((result
			      (if (class-array? type)
				  `#(,(string->symbol (class-name (class-component-type type))))
				  (string->symbol (class-name type)))))
			 (delete-local-ref type) result)) parameter-types)) 
	parameter-types-list))) ;; wtf ?



)


