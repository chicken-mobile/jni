#>
#ifndef __ANDROID__
#include "jni-adapter.h"
#else
#include <jni.h>
#endif
<#

(module jni-lolevel
        *
        (import chicken scheme foreign srfi-1 data-structures extras moremacros srfi-18)
        (import-for-syntax chicken numbers scheme)
        (use lolevel foreigners srfi-13 srfi-1 data-structures numbers lru-cache)

        (include "jni-primitives.scm")
        (include "jni-defs.scm")
        (include "jni-jlambda.scm")
        (include "jni-reflection.scm")

        (cond-expand 
          (android)
          (else
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
              (jboolean ignoreUnrecognized jvm-init-args-options-ignore-unrecognized 
                        jvm-init-args-options-ignore-unrecognized-set!))

            (define jvm-get-default-init-args
              (foreign-lambda jint JNI_GetDefaultJavaVMInitArgs jvm-init-args))

            (define jvm-create
              (foreign-lambda jint jvm_create (c-pointer java-vm) (c-pointer (c-pointer void)) c-string c-string))

            (define (jvm-init-lolevel #!optional (class-path ".") (stack-size "2m"))
              (let ((class-path-option (string-append "-Djava.class.path=" class-path))
                    (stack-option      (string-append "-Xss" stack-size)))
                (let-location ((jvm java-vm)
                               (env jni-env))
                              (jvm-create (location jvm) (location env) class-path-option stack-option)
                              (java-vm jvm)
                              (jni-env env)))))))
