(define-call-method-procs)
(define-get-field-procs)
(define-jni-modifier-procs)

(define get-field
  (jni-env-lambda jfield-id GetFieldID jclass (const c-string) (const c-string)))
(define get-static-field
  (jni-env-lambda jfield-id GetStaticFieldID jclass (const c-string) (const c-string)))
(define find-class
  (jni-env-lambda jclass FindClass (const c-string)))
(define get-object-class
  (jni-env-lambda jclass GetObjectClass jobject))
(define get-method-id
  (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string)))
(define get-static-method-id
  (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string)))
(define make-array
  (jni-env-lambda jobject-array NewObjectArray jsize jclass jobject))
(define array-length
  (jni-env-lambda jsize GetArrayLength jarray))
(define array-ref
  (jni-env-lambda jobject GetObjectArrayElement jobject-array jsize))
(define array-set!
  (jni-env-lambda void SetObjectArrayElement jobject-array jsize jobject))
(define from-reflected-method
  (jni-env-lambda jmethod-id FromReflectedMethod jobject))
(define new-object
  (jni-env-lambda jobject NewObject jclass jmethod-id))
(define jvm-attach-current-thread
  (foreign-lambda* int ((java-vm jvm)
			((c-pointer jni-env) env))
    "C_return((*jvm)->AttachCurrentThread(jvm, env, NULL));"))
(define jvm-detach-current-thread
  (foreign-lambda* int ((java-vm jvm))
    "C_return((*jvm)->DetachCurrentThread(jvm));"))

(define make-jvalue-array
  (foreign-lambda jvalue make_jvalue_array int))
(define method-id->Method
  (jni-env-lambda jobject ToReflectedMethod jclass jmethod-id jboolean))
(define Method->method-id
  (jni-env-lambda jobject FromReflectedMethod jobject))
(define field-id->Field
  (jni-env-lambda jobject ToReflectedField jclass jfield-id jboolean))
(define Field->field-id
  (jni-env-lambda jobject FromReflectedField jobject))
