(define-call-method-procs)
(define-get-field-procs)
(define-jni-modifier-procs)

(define jvm-attach-current-thread
  (foreign-lambda* int ((java-vm jvm)
			((c-pointer jni-env) env))
    "C_return((*jvm)->AttachCurrentThread(jvm, env, NULL));"))
(define jvm-detach-current-thread
  (foreign-lambda* int ((java-vm jvm))
    "C_return((*jvm)->DetachCurrentThread(jvm));"))


(define find-class
  (jni-env-lambda jclass FindClass (const c-string)))
(define get-object-class
  (jni-env-lambda jclass GetObjectClass jobject))
(define new-object
  (jni-env-lambda jobject NewObject jclass jmethod-id))

(define get-field
  (jni-env-lambda jfield-id GetFieldID jclass (const c-string) (const c-string)))
(define get-static-field
  (jni-env-lambda jfield-id GetStaticFieldID jclass (const c-string) (const c-string)))
(define get-method-id
  (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string)))
(define get-static-method-id
  (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string)))

(define make-jvalue-array
  (foreign-lambda jvalue make_jvalue_array int))
(define free-jvalue-array
  (foreign-lambda void free_jvalue_array jvalue))

(define make-array
  (jni-env-lambda jobject-array NewObjectArray jsize jclass jobject))
(define array-length
  (jni-env-lambda jsize GetArrayLength jarray))
(define array-ref
  (jni-env-lambda jobject GetObjectArrayElement jobject-array jsize))
(define array-set!
  (jni-env-lambda void SetObjectArrayElement jobject-array jsize jobject))

(define new-local-ref
  (jni-env-lambda void NewLocalRef jobject))
(define delete-local-ref
  (jni-env-lambda void DeleteLocalRef jobject))
(define new-global-ref
  (jni-env-lambda jobject NewGlobalRef jobject))
(define delete-global-ref
  (jni-env-lambda void DeleteGlobalRef jobject))

(define exception-check
  (jni-env-lambda jboolean ExceptionCheck))
(define exception-clear
  (jni-env-lambda void ExceptionClear))
(define exception-describe
  (jni-env-lambda void ExceptionDescribe))
(define exception-occurred
  (jni-env-lambda jthrowable ExceptionOccurred))

(define field-id->Field
  (jni-env-lambda jobject ToReflectedField jclass jfield-id jboolean))
(define Field->field-id
  (jni-env-lambda jobject FromReflectedField jobject))
(define method-id->Method
  (jni-env-lambda jobject ToReflectedMethod jclass jmethod-id jboolean))
(define Method->method-id
  (jni-env-lambda jobject FromReflectedMethod jobject))

(define jstring
  (jni-env-lambda jstring NewStringUTF c-string))
(define jstring->string
  (let ((get-chars     (jni-env-lambda (c-pointer (const char)) GetStringUTFChars jstring c-pointer))
        (release-chars (jni-env-lambda void ReleaseStringUTFChars jstring (c-pointer (const char))))
        (get-length    (jni-env-lambda jsize GetStringUTFLength jstring)))
    (lambda (jstring)
      (let* ((chars (get-chars jstring #f))
             (len   (get-length jstring))
             (str   (make-string len)))
        (move-memory! chars str len)
        (release-chars jstring chars)
        str))))
