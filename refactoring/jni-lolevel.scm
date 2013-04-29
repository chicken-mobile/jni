#>
#include <jni.h>
#include "jvalue-tools.c"
<#

(module jni-lolevel
*
(import scheme chicken foreign srfi-13 data-structures)
(use foreigners lolevel)
(include "jni-def-macros.scm")

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
  (jni-env-lambda jobject FromReflectedField jobject))

;; Methods
(define get-method-id
  (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string)))
(define get-static-method-id
  (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string)))
(define method-id->Method
  (jni-env-lambda jobject ToReflectedMethod jclass jmethod-id bool))
(define Method->method-id
  (jni-env-lambda jobject FromReflectedMethod jobject))

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

;; JVM
;; (define jvm-destroy
;;   (foreign-struct-lambda int DestroyJavaVM java-vm))
;; (define jvm-env
;;   (foreign-struct-lambda int GetEnv java-vm (c-pointer (c-pointer void)) int))
;; (define jvm-attach-current-thread
;;   (cond-expand
;;    (android (foreign-struct-lambda int AttachCurrentThread java-vm (c-pointer "struct JNINativeInterface const **")))
;;    (else    (foreign-struct-lambda int AttachCurrentThread java-vm (c-pointer (c-pointer void))))))
;; (define jvm-dettach-current-thread
;;   (foreign-struct-lambda int DettachCurrentThread java-vm (c-pointer (c-pointer void))))


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
  (int version jvm-init-args-version jvm-init-args-version-set!)
  (int nOptions jvm-init-args-options-length jvm-init-args-options-length-set!)
  (jvm-option options jvm-init-args-options jvm-init-args-options-set!)
  (bool ignoreUnrecognized jvm-init-args-options-ignore-unrecognized 
	jvm-init-args-options-ignore-unrecognized-set!))

(define jvm-get-default-init-args
  (foreign-lambda int JNI_GetDefaultJavaVMInitArgs jvm-init-args))

(define jvm-create
  (foreign-lambda int JNI_CreateJavaVM (c-pointer java-vm) (c-pointer (c-pointer void)) jvm-init-args))

(define (jvm-init #!optional (class-path "."))
  (let ((args (make-jvm-init-args))
	;;      (class-path-option (make-jvm-option))
	(stack-size-option (make-jvm-option)))

    (jvm-init-args-version-set! args JNI_VERSION_1_6)
    (jvm-get-default-init-args args)
    (jvm-init-args-options-length-set! args 1)
    (jvm-init-args-options-set! args stack-size-option)
    ;;  (jvm-option-string-set! class-path-option (string-append "-Djava.class.path=" class-path))
    (jvm-option-string-set! stack-size-option (string-append "-Xss10M" ))

    (let-location ((jvm (c-pointer java-vm))
		   (env (c-pointer jni-env)))
      (jvm-create (location jvm) (location env) args)
      (java-vm jvm)
      (jni-env env))))

;; Helper
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

(define (array->list array-object)

  (do ((idx 0 (+ idx 1))
       (object-list '() (cons (array-ref array-object idx) object-list)))
      ((<= (array-length array-object) idx) object-list)))

(define (list->array class lst)
  (let ((arr (make-array (length lst) class #f)))
    (let loop ((i 0) (lst lst))
      (if (null? lst) arr
	  (begin
	    (array-set! arr i (car lst))
	    (loop (+ i 1) (cdr lst)))))))

;; Debug
(define to-string
  (lambda (object)
    (let* ((Object.toString/method (get-method-id (find-class "java/lang/Object") "toString" "()Ljava/lang/String;"))
           (String/instance (call-object-method object Object.toString/method #f))
           (string (jstring->string String/instance)))
      (delete-local-ref String/instance) string)))
)
