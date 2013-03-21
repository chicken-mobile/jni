#>
#include <jni.h>
<#

(module jni-jvm
        *
        (import chicken scheme foreign srfi-1 data-structures extras)
        (import-for-syntax chicken)
        (use lolevel foreigners)

        (include "jni-primitives.scm")
        (include "jni-defs.scm")
        (include "jni-jlambda.scm")
        (include "jni-reflection.scm")
        
(define-foreign-record-type (jvm-init-args "JavaVMInitArgs")
  (constructor: make-jvm-init-args)
  (destructor: free-jvm-init-args)
  (jint version jvm-init-args-version jvm-init-args-version-set!)
  (jint nOptions jvm-init-args-options-length jvm-init-args-options-length-set!)
  (jvm-option options jvm-init-args-options jvm-init-args-options-set!)
  (jboolean ignoreUnrecognized jvm-init-args-options-ignore-unrecognized jvm-init-args-options-ignore-unrecognized-set!))

(define jvm-get-default-init-args
  (foreign-lambda jint JNI_GetDefaultJavaVMInitArgs jvm-init-args))

(define jvm-create
  (foreign-lambda jint JNI_CreateJavaVM (c-pointer java-vm) (c-pointer (c-pointer void)) jvm-init-args))

(define (jvm-init #!optional (class-path "."))
  (let ((args (make-jvm-init-args))
        (class-path-option (make-jvm-option)))

    (jvm-init-args-version-set! args JNI_VERSION_1_6)
    (jvm-init-args-options-length-set! args 1)
    (jvm-init-args-options-set! args class-path-option)
    (jvm-option-string-set! class-path-option (string-append "-Djava.class.path=" class-path))

    (let-location ((jvm java-vm)
                   (env jni-env))
                  (jvm-create (location jvm) (location env) args)
                  (java-vm jvm)
                  (jni-env env))))

)