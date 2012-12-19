#>
#include <jni.h>
<#

(module jni
*

(import chicken scheme foreign)
(import-for-syntax chicken data-structures)
(use lolevel)

(include "jni-types.scm")
(include "jni-def-macros.scm")
(include "jni-defs.scm")

(define jni-env
  (make-parameter #f))

(define jstring
  (jni-env-lambda jstring NewStringUTF c-string))

(define (list->array class lst)
  (let ((arr (make-array (length lst) class #f)))
    (let loop ((i 0) (lst lst))
      (if (null? lst)
          arr
          (begin
            (array-set! arr i (car lst))
            (loop (+ i 1) (cdr lst)))))))

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
     (fn (class class-name)
         (symbol->string 'name)
         (type-signature (args ...) return)))))

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

(define-for-syntax (call-type-method/n type n static?)
  (string->symbol
   (string-append "call-"
                  (if static? "static-" "")
                  (symbol->string type)
                  "-method/"
                  (number->string n))))

(define-for-syntax (dispatch-method-call type nargs)
  `(apply
    (if static?
        ,(call-type-method/n type nargs #t)
        ,(call-type-method/n type nargs #f))
    obj method args))

(define java-vm
  (make-parameter #f))

(define (with-jvm-thread jvm proc)
  (let-location ((env jni-env))
    (jvm-attach-current-thread jvm (location env))
    (parameterize ((jni-env env) (java-vm jvm)) (proc))
    (jvm-detach-current-thread jvm)))
(define make-array
  (jni-env-lambda jobject-array NewObjectArray jsize jclass jobject))

(define array-length
  (jni-env-lambda jsize GetArrayLength jarray))

(define array-ref
  (jni-env-lambda jobject GetObjectArrayElement jobject-array jsize))

(define array-set!
  (jni-env-lambda void SetObjectArrayElement jobject-array jsize jobject))

(define (list->array class lst)
  (let ((arr (make-array (length lst) class #f)))
    (let loop ((i 0) (lst lst))
      (if (null? lst)
          arr
          (begin
            (array-set! arr i (car lst))
            (loop (+ i 1) (cdr lst)))))))


(define (array->list array-object)
  (do ((idx 0 (+ idx 1))
       (object-list '() (cons (array-ref array-object idx) object-list)))
      ((<= (array-length array-object) idx) object-list)))
)
