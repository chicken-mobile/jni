(define-foreign-type java-vm (c-pointer "JavaVM"))
(define-foreign-type jni-env (c-pointer "JNIEnv"))
(define-foreign-type jint int)
(define-foreign-type jobject (c-pointer "struct _jobject"))
(define-foreign-type jclass jobject)
(define-foreign-type jstring jobject)
(define-foreign-type jmethod-id (c-pointer (struct "_jmethodID")))
(define-foreign-type jfield-id (c-pointer (struct "_jfieldID")))
(define-foreign-type jsize jint)
(define-foreign-type jarray jobject)
(define-foreign-type jobject-array jarray)
(define-foreign-type jvalue (c-pointer (union "jvalue")))
(define-foreign-type jvoid int)
(define-foreign-type jboolean bool)
(define-foreign-type jbyte char)
(define-foreign-type jchar unsigned-short char->integer integer->char)
(define-foreign-type jshort short)
(define-foreign-type jlong integer64)
(define-foreign-type jfloat float)
(define-foreign-type jdouble double)
