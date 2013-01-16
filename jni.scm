#>
#include <jni.h>
<#

(module jni
*

(import chicken scheme foreign)
(import-for-syntax chicken data-structures)
(use lolevel foreigners)

(include "jni-types.scm")
(include "jni-def-macros.scm")
(include "jni-defs.scm")

(define jni-env
  (make-parameter #f))
(define java-vm
  (make-parameter #f))

(define-for-syntax (mangle-class-name name)
  (string-translate (symbol->string name) #\. #\/))

(define-syntax class
  (ir-macro-transformer
   (lambda (x i c)
     (let ((name (mangle-class-name (strip-syntax (cadr x)))))
       `(find-class ,name)))))

(define-for-syntax (expand-type type #!optional return)
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
         (and-let* ((return (expand-type return)))
           (string-append
            "(" (string-intersperse (map expand-type type) "") ")"
            return)))
        (else #f)))

(define-syntax type-signature
  (er-macro-transformer
   (lambda (x r c)
     (let ((type (cadr x)))
       (or (expand-type type (and (pair? (cddr x)) (caddr x)))
           (error "Invalid Java type signature" x))))))

(define-syntax method*
  (syntax-rules ()
    ((_ fn class-name return name args ...)
     (let* ((class-object (class class-name))
	    (return-value (fn class-object
			      (symbol->string 'name)
			      (type-signature (args ...) return))))
       (delete-local-ref class-object)
       return-value))))

(define-syntax method
  (syntax-rules ()
    ((_ args ...)
     (method* get-method-id args ...))))

(define-syntax static-method
  (syntax-rules ()
    ((_ args ...)
     (method* get-static-method-id args ...))))

(define-syntax constructor
  (er-macro-transformer
   (lambda (x r c)
     `(,(r 'method) ,(cadr x) void <init> . ,(cddr x)))))

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
          (parameterize ((jni-env env))
            . ,(cdddr x)))))))

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


(define (array->list array-object)
  (do ((idx 0 (+ idx 1))
       (object-list '() (cons (array-ref array-object idx) object-list)))
      ((<= (array-length array-object) idx) object-list)))

(define (list->array class lst)
  (let ((arr (make-array (length lst) class #f)))
    (let loop ((i 0) (lst lst))
      (if (null? lst)
          arr
          (begin
            (array-set! arr i (car lst))
            (loop (+ i 1) (cdr lst)))))))


(define to-string
  (lambda (object)
    (let* ((Object.toString/method (method java.lang.Object java.lang.String toString))
	   (String/instance (call-object-method object Object.toString/method #f))
	   (string (jstring->string String/instance)))
      (delete-local-ref String/instance) string)))

(define (jprint . values)
  (for-each display
	 (map (lambda (value)
		(if (pointer? value)
		    (to-string value) value))
	      (cons values "\n"))))

)
