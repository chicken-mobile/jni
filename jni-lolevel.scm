#>
#ifndef __ANDROID__
#include "jni-adapter.h"
#else
#include <jni.h>
#endif
<#

(module jni-lolevel
*
(import chicken scheme foreign srfi-1 data-structures extras moremacros)
(import-for-syntax chicken numbers scheme)
(use lolevel foreigners srfi-13 srfi-1 data-structures numbers)

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


  (define (jvm-option prefix option vals)
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

  (define (jvm-property-option property value)
    (jvm-option "-D" (keyword->string property) (list value)))

  (define (jvm-non-standard-option property . value)
    (jvm-option "-X" (keyword->string property) value))

  (define (jvm-hotspot-option option . value)
    (jvm-option "-XX:" (keyword->string option) value))

  (define string->pointer
    (foreign-lambda (c-pointer char) string_to_pointer c-string))

  (define jvm-create
    (foreign-lambda jint jvm_create (c-pointer java-vm) (c-pointer (c-pointer void)) pointer-vector unsigned-int))

  (define (jvm-init-lolevel #!optional (class-path ".") (stack-size 2) #!rest option-strings)
    (let ((class-path-option (jvm-property-option     java.class.path: class-path))
	  (stack-option      (jvm-non-standard-option ss:              stack-size 'M)))

      (let* ((length (+ (length option-strings) 2))
	     (pv (make-pointer-vector length)))

	(fold (lambda (x i)
		(let* ((string (->string x))
		       (mem    (allocate (string-length string))))
		  (move-memory! string mem)
		  (pointer-vector-set! pv i mem) (+ i 1)))
	      0 (cons class-path-option (cons stack-option (or option-strings '()))))


	(let-location ((jvm java-vm)
		       (env jni-env))
	  (jvm-create (location jvm) (location env) pv length)
	  (java-vm jvm)
	  (jni-env env))))))))
