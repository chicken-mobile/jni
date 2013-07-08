#>
#include <jni.h>
#include "jni-adapter.h"
#include "jvalue-tools.c"
<#

(module jni-lolevel
*
(import scheme chicken foreign foreigners srfi-1 srfi-13 data-structures)
(use lolevel)

;; foreign types
(define-foreign-type java-vm    (c-pointer "JavaVM"))
(define-foreign-type jni-env    (c-pointer "JNIEnv"))

(define (testo foo)
  (new-global-ref foo))
(define (festo foo) foo)

(define-foreign-type        jobject (c-pointer "struct _jobject"))
(define-foreign-type global-jobject (c-pointer "struct _jobject"))
(define-foreign-type  local-jobject (c-pointer "struct _jobject") 
)


(define-foreign-type jmethod-id (c-pointer "struct _jmethodID"))
(define-foreign-type jfield-id  (c-pointer "struct _jfieldID"))
(define-foreign-type jvalue (c-pointer (union "jvalue")))

(define-foreign-type jclass  local-jobject)
(define-foreign-type jstring local-jobject)
(define-foreign-type jarray  local-jobject)
(define-foreign-type jobject-array jarray)
(define-foreign-type jthrowable local-jobject)

(define-foreign-enum-type (ref-type int)
  (ref-type->int int->ref-type)
  ((invalid ref-type/invalid) JNIInvalidRefType)
  ((local ref-type/local) JNILocalRefType)
  ((global ref-type/global) JNIGlobalRefType)
  ((weak ref-type/weak) JNIWeakGlobalRefType))

(define invalid-ref
  ref-type/invalid)

(include "jni-def-macros.scm")
(define-call-procs Void void)
(define-type-procs)
(define-get-field-procs)
(define-jni-modifier-procs)


;; Environment Parameters
(define jni-env
  (make-parameter #f))
(define java-vm
  (make-parameter #f))

;; jvalue
(define make-jvalue-array
  (foreign-lambda jvalue make_jvalue_array int))
(define free-jvalue-array
  (foreign-lambda void free_jvalue_array jvalue))


;; Classes
(define find-class
  (jni-env-lambda local-jobject FindClass (const c-string)))
(define super-class
  (jni-env-lambda local-jobject GetSuperclass jclass))
(define instance-of?
  (jni-env-lambda bool IsInstanceOf jobject jclass))
(define assignable-from?
  (jni-env-lambda bool IsAssignableFrom jclass jclass))

;; Objects
(define alloc-object
  (jni-env-lambda local-jobject AllocObject jclass))
(define new-object
  (jni-env-lambda local-jobject NewObjectA jclass jmethod-id jvalue))
(define same-object?
  (jni-env-lambda bool IsSameObject jobject jobject))
(define object-class
  (jni-env-lambda jclass GetObjectClass jobject))

;; Fields
(define get-field-id
  (jni-env-lambda jfield-id GetFieldID jclass (const c-string) (const c-string)))
(define get-static-field-id
  (jni-env-lambda jfield-id GetStaticFieldID jclass (const c-string) (const c-string)))
(define field-id->Field
  (jni-env-lambda local-jobject ToReflectedField jclass jfield-id bool))
(define Field->field-id
  (jni-env-lambda jfield-id FromReflectedField jobject))

;; Methods
(define get-method-id
  (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string)))
(define get-static-method-id
  (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string)))
(define method-id->Method
  (jni-env-lambda local-jobject ToReflectedMethod jclass jmethod-id bool))
(define Method->method-id
  (jni-env-lambda c-pointer FromReflectedMethod jobject))

;; Arrays
(define array-length
  (jni-env-lambda size_t GetArrayLength jarray))
(define make-array
  (jni-env-lambda jobject-array NewObjectArray size_t jclass jobject))
(define array-ref
  (jni-env-lambda local-jobject GetObjectArrayElement jobject-array size_t))
(define array-set!
  (jni-env-lambda void SetObjectArrayElement jobject-array size_t jobject))


;; GC
(define new-local-ref
  (jni-env-lambda local-jobject NewLocalRef jobject))
(define delete-local-ref
  (jni-env-lambda void DeleteLocalRef jobject))
(define new-global-ref
  (jni-env-lambda global-jobject NewGlobalRef jobject))
(define delete-global-ref
  (jni-env-lambda void DeleteGlobalRef jobject))
(define push-local-frame
  (jni-env-lambda int PushLocalFrame int))
(define pop-local-frame
  (jni-env-lambda local-jobject PopLocalFrame jobject))
(define jobject-ref-type
  (jni-env-lambda ref-type GetObjectRefType jobject))

;; Exceptions
(define exception-check
  (jni-env-lambda bool ExceptionCheck))
(define exception-clear
  (jni-env-lambda void ExceptionClear))
(define exception-describe
  (jni-env-lambda void ExceptionDescribe))
(define exception-occurred
  (jni-env-lambda jthrowable ExceptionOccurred))

;; Locking
(define monitor-enter
  (jni-env-lambda int MonitorEnter jobject))
(define monitor-exit
  (jni-env-lambda int MonitorExit jobject))


(define jvm-create
  (foreign-lambda int jvm_create (c-pointer java-vm) (c-pointer (c-pointer void)) c-string c-string))

(define (jvm-init #!optional (class-path ".") (stack-size "5m"))
  (let ((class-path-option (string-append "-Djava.class.path=" class-path))
	(stack-option      (string-append "-Xss" stack-size)))
    (let-location ((jvm java-vm) (env jni-env))
      (let* ((result (jvm-create (location jvm) (location env) class-path-option stack-option)))	
	(assert (= result 0) (format "jvm-create returned: ~A" result))) 
      (java-vm jvm) (jni-env env) jvm)))

(define jvm-attach-current-thread
  (foreign-lambda* int ((java-vm jvm) ((c-pointer (c-pointer void))  env))
    "C_return((*jvm)->AttachCurrentThread(jvm, env, NULL));"))

(define (attach-thread #!optional (class-path "."))
  (let ((jvm (or (java-vm) (jvm-init class-path))))
    (unless (jni-env)
      (let-location ((env jni-env))
	(let ((result (jvm-attach-current-thread jvm (location env))))
	  (assert (= result 0) (error "jvm-attach-current-thread:" result))
	  (jni-env env))))))

(define-syntax jni-init
  (syntax-rules ()
    ((_)
     (foreign-declare "
#include <jni.h>

static JavaVM* jvm;

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *vm, void *reserved)
{
 jvm = vm;

     CHICKEN_run(C_toplevel);
     return JNI_VERSION_1_6;
     }"))))

(define-for-syntax (mangle-method-name name)
  (string->symbol
   (string-append "Java_" (string-translate (symbol->string name) #\. #\_))))

(define-syntax define-method
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((name (mangle-method-name (strip-syntax (caadr x)))))
       `(define-external (,(i name)
			  (,(i '(c-pointer "JNIEnv")) env)
			  (,(i '(c-pointer "jobject")) ,(cadadr x))
			  . ,(cddadr x))
	  ,(i (caddr x))
	  (parameterize ((jni-env env)) . ,(cdddr x)))))))

;; Helper
(define jstring
  (jni-env-lambda jstring NewStringUTF c-string))

(define jstring->string
  (let ((get-chars     (jni-env-lambda (c-pointer (const char)) GetStringUTFChars jstring c-pointer))
        (release-chars (jni-env-lambda void ReleaseStringUTFChars jstring (c-pointer (const char))))
        (get-length    (jni-env-lambda size_t GetStringUTFLength jstring)))
    (lambda (jstring)
      (let* ((chars (get-chars jstring #f))
             (len   (get-length jstring))
             (str   (make-string len)))
        (move-memory! chars str len)
        (release-chars jstring chars)
        str))))

(define (jstring->string! jstring)
  (let ((result (jstring->string jstring)))
    (delete-local-ref jstring) result))

;; Debug
(define to-string
  (lambda (object)
    (let* ((Object.toString/method (get-method-id (find-class "java/lang/Object") "toString" "()Ljava/lang/String;"))
           (String/instance (call-object-method object Object.toString/method #f))
           (string (jstring->string String/instance)))
      (delete-local-ref String/instance) string)))

)
