#>
#include <jni.h>
<#

(module jni
*

(import chicken scheme foreign)
(import-for-syntax chicken data-structures)
(use lolevel foreigners)

(include "jni-primitives.scm")
(include "jni-defs.scm")
(include "jni-java-utils.scm")

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

(define jni-env
  (make-parameter #f))
(define java-vm
  (make-parameter #f))

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
