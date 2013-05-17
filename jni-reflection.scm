;; jni-reflection: high level java procedures

(define Class.isPrimitive
  (jlambda-method #f boolean java.lang.Class isPrimitive))
(define Class.getFields
  (jlambda-method #f #(java.lang.reflect.Field) java.lang.Class getFields))
(define Class.getDeclaredFields
  (jlambda-method #f #(java.lang.reflect.Field) java.lang.Class getDeclaredFields))
(define Class.getMethod
  (jlambda-method #f java.lang.reflect.Method java.lang.Class getMethod java.lang.String #(java.lang.Class)))
(define Class.getMethods
  (jlambda-method #f #(java.lang.reflect.Method) java.lang.Class getMethods))
(define Class.getConstructors
  (jlambda-method #f #(java.lang.reflect.Constructor) java.lang.Class getConstructors))
(define Class.getDeclaredMethods
  (jlambda-method #f #(java.lang.reflect.Method) java.lang.Class getDeclaredMethods))

(define Field.getName
  (jlambda-method #f java.lang.String java.lang.reflect.Field getName))
(define Field.getType
  (jlambda-method #f java.lang.Class java.lang.reflect.Field getType))
(define Field.getModifiers
  (jlambda-method #f int java.lang.reflect.Field getModifiers))

(define Method.invoke
  (jlambda-method #f java.lang.Object java.lang.reflect.Method invoke java.lang.Object #(java.lang.Object)))
(define Method.getModifiers
  (jlambda-method #f int java.lang.reflect.Method getModifiers))
(define Method.getReturnType
  (jlambda-method #f java.lang.Class java.lang.reflect.Method getReturnType))
(define Method.getName
  (jlambda-method #f java.lang.String java.lang.reflect.Method getName))
(define Method.getParameterTypes
  (jlambda-method #f #(java.lang.Class) java.lang.reflect.Method getParameterTypes))

(define Constructor.getParameterTypes
  (jlambda-method #f #(java.lang.Class) java.lang.reflect.Constructor getParameterTypes))

(define find-field/helper
  (jlambda-method (static) java.lang.reflect.Field 
                  com.chicken_mobile.jni.ReflectionHelper findField java.lang.Class java.lang.String))

(define find-methods/helper 
  (jlambda-method (static) #(java.lang.reflect.Method)
                  com.chicken_mobile.jni.ReflectionHelper findMethods java.lang.Class java.lang.String))
