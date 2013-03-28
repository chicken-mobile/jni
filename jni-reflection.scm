;; jni-reflection: high level java procedures

(define Class.isPrimitive
  (jlambda-method* #f boolean java.lang.Class isPrimitive))
(define Class.getMethods
  (jlambda-method* #f #(java.lang.reflect.Method) java.lang.Class getMethods))
(define Class.getDeclaredMethods
  (jlambda-method* #f #(java.lang.reflect.Method) java.lang.Class getDeclaredMethods))

(define Method.getModifiers
  (jlambda-method* #f int java.lang.reflect.Method getModifiers))
(define Method.getReturnType
  (jlambda-method* #f java.lang.Class java.lang.reflect.Method getReturnType))
(define Method.getName
  (jlambda-method* #f java.lang.String java.lang.reflect.Method getName))
(define Method.getParameterTypes
  (jlambda-method* #f #(java.lang.Class) java.lang.reflect.Method getParameterTypes))

(define find-field/helper
  (jlambda-method* (static) java.lang.reflect.Field 
                  com.chicken_mobile.jni.ReflectionHelper findField java.lang.Class java.lang.String))

(define find-methods/helper
  (jlambda-method* (static) #(java.lang.reflect.Method)
                  com.chicken_mobile.jni.ReflectionHelper findMethods java.lang.Class java.lang.String))

(define Field.getType
  (jlambda-method* #f java.lang.Class java.lang.reflect.Field getType))

(define Field.getModifiers
  (jlambda-method* #f int java.lang.reflect.Field getModifiers))
