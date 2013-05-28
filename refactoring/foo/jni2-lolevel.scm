#>
#include <jni.h>
#include "jni-adapter.h"
#include "jvalue-tools.c"
<#

(module jni2-lolevel
*
(import scheme chicken foreign foreigners srfi-1 srfi-13 data-structures)
(use lolevel)

;; foreign types
(define-foreign-type java-vm    (c-pointer "JavaVM"))
(define-foreign-type jni-env    (c-pointer "JNIEnv"))
(define-foreign-type jobject    (c-pointer "struct _jobject"))
(define-foreign-type jmethod-id (c-pointer "struct _jmethodID"))
(define-foreign-type jfield-id  (c-pointer "struct _jfieldID"))
(define-foreign-type jvalue (c-pointer (union "jvalue")))

(define-foreign-type jclass  jobject)
(define-foreign-type jstring jobject)
(define-foreign-type jarray  jobject)
(define-foreign-type jobject-array jarray)
(define-foreign-type jthrowable jobject)

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


(define (expand-type type #!optional return-type)
  (cond ((symbol? type)
	 (case type
	   ((boolean) "Z")
	   ((byte)    "B")
	   ((char)    "C")
	   ((short)   "S")
	   ((int)     "I")
	   ((long)    "J")
	   ((float)   "F")
	   ((double)  "D")
	   ((void)    "V")
	   (else (string-append "L" (mangle-class-name type) ";"))))
	((vector? type)
	 (string-append "[" (expand-type (vector-ref type 0))))
	((list? type)
	 (and-let* ((return (expand-type return-type)))
	   (string-append "(" (string-intersperse (map expand-type type) "") ")" return)))))

(define (mangle-class-name name)
  (cond
   ((symbol? name)
    ;; i think this is wrong ... it suggests you can get a primitve type class object,
    ;; which you cant do with find-class. I think it was hack to compare argument types
    ;; more easily because is-assignable-from would return true for them in this way.
    ;; Also they should be retrieved somewhere to map them to the actual primitve Class.
    (case name 
      ((boolean) "java/lang/Boolean")
      ((byte)    "java/lang/Byte")
      ((char)    "java/lang/Character")
      ((short)   "java/lang/Short")
      ((int)     "java/lang/Integer")
      ((long)    "java/lang/Long")
      ((float)   "java/lang/Float")
      ((double)  "java/lang/Double")
      ((void)    "java/lang/Void")
      (else (string-translate (symbol->string name) #\. #\/))))
   ((vector? name)
    (expand-type name))))

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
  (jni-env-lambda jclass FindClass (const c-string)))
(define super-class
  (jni-env-lambda jclass GetSuperclass jclass))
(define instance-of?
  (jni-env-lambda bool IsInstanceOf jobject jclass))
(define assignable-from?
  (jni-env-lambda bool IsAssignableFrom jclass jclass))

;; Objects
(define new-object
  (jni-env-lambda jobject NewObjectA jclass jmethod-id jvalue))
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
  (jni-env-lambda jobject ToReflectedField jclass jfield-id bool))
(define Field->field-id
  (jni-env-lambda jfield-id FromReflectedField jobject))

;; Methods
(define get-method-id
  (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string)))
(define get-static-method-id
  (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string)))
(define method-id->Method
  (jni-env-lambda jobject ToReflectedMethod jclass jmethod-id bool))
(define Method->method-id
  (jni-env-lambda c-pointer FromReflectedMethod jobject))

;; Arrays
(define make-array
  (jni-env-lambda jobject-array NewObjectArray size_t jclass jobject))
(define array-length
  (jni-env-lambda size_t GetArrayLength jarray))
(define array-ref
  (jni-env-lambda jobject GetObjectArrayElement jobject-array size_t))
(define array-set!
  (jni-env-lambda void SetObjectArrayElement jobject-array size_t jobject))

;; GC
(define new-local-ref
  (jni-env-lambda jobject NewLocalRef jobject))
(define delete-local-ref
  (jni-env-lambda void DeleteLocalRef jobject))
(define new-global-ref
  (jni-env-lambda jobject NewGlobalRef jobject))
(define delete-global-ref
  (jni-env-lambda void DeleteGlobalRef jobject))
(define push-local-frame
  (jni-env-lambda int PushLocalFrame int))
(define pop-local-frame
  (jni-env-lambda jobject PopLocalFrame jobject))
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

(define (jvm-init #!optional (class-path ".") (stack-size "20m"))
  (let ((class-path-option (string-append "-Djava.class.path=" class-path))
	(stack-option      (string-append "-Xss" stack-size)))
    (let-location ((jvm java-vm)
		   (env jni-env))
      (jvm-create (location jvm) (location env) class-path-option stack-option)

      (java-vm jvm)
      (jni-env env))))

(define jvm-attach-current-thread
  (foreign-lambda* int ((java-vm jvm) ((c-pointer (c-pointer void)) env))
    "C_return( (*(JavaVM*)jvm)->AttachCurrentThread(jvm, env, NULL));"))



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

(unless (jni-env)
  (jvm-init "./"))

)
