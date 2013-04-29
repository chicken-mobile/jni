#>
#include <jni.h>
<#

(use foreigners data-structures srfi-13)


(define-foreign-type java-vm (c-pointer "struct JNIInvokeInterface_"))
(define-foreign-type jni-env (c-pointer "struct JNINativeInterface_"))
(define-foreign-type jvm-options c-pointer)


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
  (jvm-options options jvm-init-args-options jvm-init-args-options-set!)
  (bool ignoreUnrecognized jvm-init-args-options-ignore-unrecognized 
       jvm-init-args-options-ignore-unrecognized-set!))

(define jvm-default-init-args
  (foreign-lambda int JNI_GetDefaultJavaVMInitArgs (c-pointer jvm-init-args)))
(define jvm-create
  (foreign-lambda int JNI_CreateJavaVM (const (c-pointer (c-pointer java-vm))) (c-pointer (c-pointer void)) (c-pointer jvm-init-args)))

(define-foreign-variable JNI-1.1 int JNI_VERSION_1_1)
(define-foreign-variable JNI-1.2 int JNI_VERSION_1_2)
(define-foreign-variable JNI-1.4 int JNI_VERSION_1_4)
(define-foreign-variable JNI-1.6 int JNI_VERSION_1_6)


(define java-vm
  (make-parameter #f))
(define jni-env
  (make-parameter #f))


(define (set-jvm-arg! prefix option vals)
  (string-append
   (or prefix "-")
   (if (null? vals)
       option
       (let ((val (car vals)))
         (cond ((number? val)
                (string-append option
                               (number->string val)
                               (let ((unit (and (pair? (cdr vals)) (cadr vals))))
                                 (if unit (->string unit) ""))))
               ((boolean? val)
                (string-append (if val "+" "-") option))
               (else
                (string-append option "=" (->string val))))))))

(define (set-jvm-property-arg! property value)
  (set-jvm-arg! "-D" (keyword->string property) (list value)))

(define (set-jvm-non-standard-arg! property . value)
  (set-jvm-arg! "-X" (keyword->string property) value))

(define (set-jvm-hotspot-arg! option . value)
  (set-jvm-arg! "-XX:" (keyword->string option) value))

(define (jvm-init*)
  (let ((args (make-jvm-init-args))
	(stack-size-option (make-jvm-option)))

      (jvm-init-args-version-set!        args JNI-1.6)
      (jvm-init-args-options-length-set! args 1)
      (jvm-init-args-options-set!        args stack-size-option)
      (jvm-option-string-set! stack-size-option (string-append "-Xss2M" ))

      (let-location ((jvm (c-pointer java-vm))
		     (env (c-pointer jni-env)))
	(jvm-create (location jvm) (location env) args)
	(java-vm jvm)
	(jni-env env))))

(print (set-jvm-property-arg! java.class.path: "."))
(print (set-jvm-non-standard-arg! check: 'jni))
(print (set-jvm-hotspot-arg! 'PrintCompilation))
(print (set-jvm-hotspot-arg! '+PrintCompilation))
(print (set-jvm-hotspot-arg! '-PrintCompilation))
(print (set-jvm-hotspot-arg! PredictedClassLoadCount: '123456789M))
(print (set-jvm-non-standard-arg! ss: '12345M))



