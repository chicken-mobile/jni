;; generate type variant procedures
(include "jni-def-macros.scm")
(define-call-procs Void void)
(define-type-procs)
(define-get-field-procs)
(define-jni-modifier-procs)
;;

(define-foreign-variable JNI_VERSION_1_1 int)
(define-foreign-variable JNI_VERSION_1_2 int)
(define-foreign-variable JNI_VERSION_1_4 int)
(define-foreign-variable JNI_VERSION_1_6 int)

(define-foreign-record-type (jvm-option "JavaVMOption")
  (constructor: make-jvm-option)
  (destructor: free-jvm-option)
  (c-string  optionString jvm-option-string jvm-option-string-set!)
  ((c-pointer void) extraInfo jvm-option-info jvm-option-info-set!))
(define-foreign-record-type (jvm-init-args "JavaVMInitArgs")
  (constructor: make-jvm-init-args)
  (destructor: free-jvm-init-args)
  (jint version jvm-init-args-version jvm-init-args-version-set!)
  (jint nOptions jvm-init-args-options-length jvm-init-args-options-length-set!)
  (jvm-option options jvm-init-args-options jvm-init-args-options-set!)
  (jboolean ignoreUnrecognized jvm-init-args-options-ignore-unrecognized jvm-init-args-options-ignore-unrecognized-set!))

(define version
  (jni-env-lambda jint GetVersion))
(define jvm-get-default-init-args
  (foreign-lambda jint JNI_GetDefaultJavaVMInitArgs jvm-init-args))
(define jvm-create
  (foreign-lambda jint JNI_CreateJavaVM (c-pointer java-vm) (c-pointer (c-pointer void)) jvm-init-args))
(define jvm-destroy
  (foreign-lambda* jint ((java-vm jvm))
    "C_return((*jvm)->DestroyJavaVM(jvm));"))
(define jvm-env
  (foreign-lambda* jint ((java-vm jvm) ((c-pointer (c-pointer void)) env) (jint version))
    "C_return((*jvm)->GetEnv(jvm, env, version));"))
(define jvm-attach-current-thread
  (foreign-lambda* int ((java-vm jvm) ((c-pointer (c-pointer void)) env))
    "C_return((*jvm)->AttachCurrentThread(jvm, env, NULL));"))
(define jvm-detach-current-thread
  (foreign-lambda* int ((java-vm jvm))
    "C_return((*jvm)->DetachCurrentThread(jvm));"))


(define find-class
  (jni-env-lambda jclass FindClass (const c-string)))
(define super-class
  (jni-env-lambda jclass GetSuperclass jclass))
(define get-object-class
  (jni-env-lambda jclass GetObjectClass jobject))
(define instance-of?
  (jni-env-lambda jboolean IsInstanceOf jobject jclass))
(define same-object?
  (jni-env-lambda jboolean IsSameObject jobject jobject))
(define assignable-from?
  (jni-env-lambda jboolean IsAssignableFrom jclass jclass))
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
  (jni-env-lambda jobject NewLocalRef jobject))
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

(define monitor-enter
  (jni-env-lambda jint MonitorEnter jobject))
(define monitor-exit
  (jni-env-lambda jint MonitorExit jobject))

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

(define-syntax jni-init
  (syntax-rules ()
    ((_)
     (foreign-declare "
#include <jni.h>

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *vm, void *reserved)
{
 CHICKEN_run(C_toplevel);
 return JNI_VERSION_1_6;
}"))))
