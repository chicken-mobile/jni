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

(define exception-check
  (jni-env-lambda jboolean ExceptionCheck))
(define exception-clear
  (jni-env-lambda void ExceptionClear))
(define exception-describe
  (jni-env-lambda void ExceptionDescribe))
(define exception-occurred
  (jni-env-lambda jthrowable ExceptionOccurred))

(define new-local-ref
  (jni-env-lambda void NewLocalRef jobject))
(define delete-local-ref
  (jni-env-lambda void DeleteLocalRef jobject))

(define new-global-ref
  (jni-env-lambda jobject NewGlobalRef jobject))
(define delete-global-ref
  (jni-env-lambda void DeleteGlobalRef jobject))

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


(define null-args (make-jvalue-array 0))

(define (primitive? Class/instance)
  (let* ((Class.isPrimitive/method (method java.lang.Class boolean isPrimitive))
	 (is-primitive             (call-boolean-method Class/instance Class.isPrimitive/method null-args)))
    is-primitive))

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

(define (reflected-field-modifiers Field/instance)
  (let* ((Field.getModifiers/method (method java.lang.reflect.Field int getModifiers))
	 (modifiers                 (call-int-method Field/instance Field.getModifiers/method null-args)))
    modifiers))

(define (reflected-field-type Field/instance)
  (let* ((Field.getType/method (method java.lang.reflect.Field java.lang.Class getType))
	 (Class/instance       (call-object-method Field/instance Field.getType/method null-args)))
    Class/instance))

(define (reflected-field object field-name)
  (let* ((object-class (get-object-class object))
	 (field-name (jstring (symbol->string field-name)))
	 (Class.getDeclaredField/method (method java.lang.Class java.lang.reflect.Field getDeclaredField java.lang.String))
	 (args (make-jvalue-array 1))
	 (Field/instance (call-object-method object-class Class.getDeclaredField/method
					     (begin
					       (set-object-jvalue! args 0 field-name) args))))
    (delete-local-ref field-name)
    (delete-local-ref object-class)
    Field/instance))

(define (field object field-name)
  (let* ((Field/instance       (reflected-field object    field-name))
	 (Field.type/Class     (reflected-field-type      Field/instance))
	 (Field.modifiers/int  (reflected-field-modifiers Field/instance)))

    (let ((return-value
	   (if (primitive? Field.type/Class)
	       (case (string->symbol (to-string Field.type/Class))
		 ((byte)
		  (if (static? Field.modifiers/int)
		      (get-static-byte-field    object (Field->field-id Field/instance))
		      (get-byte-field           object (Field->field-id Field/instance))))
		 ((short)
		  (if (static? Field.modifiers/int)
		      (get-static-short-field   object (Field->field-id Field/instance))
		      (get-short-field          object (Field->field-id Field/instance))))
		 ((int)
		  (if (static? Field.modifiers/int)
		      (get-static-int-field     object (Field->field-id Field/instance))
		      (get-int-field            object (Field->field-id Field/instance))))
		 ((long)
		  (if (static? Field.modifiers/int)
		      (get-static-long-field    object (Field->field-id Field/instance))
		      (get-long-field           object (Field->field-id Field/instance))))
		 ((float)
		  (if (static? Field.modifiers/int)
		      (get-static-float-field   object (Field->field-id Field/instance))
		      (get-float-field          object (Field->field-id Field/instance))))
		 ((double)
		  (if (static? Field.modifiers/int)
		      (get-static-double-field  object (Field->field-id Field/instance))
		      (get-double-field         object (Field->field-id Field/instance))))
		 ((char)
		  (if (static? Field.modifiers/int)
		      (get-static-char-field    object (Field->field-id Field/instance))
		      (get-char-field           object (Field->field-id Field/instance))))
		 ((boolean)
		  (if (static? Field.modifiers/int)
		      (get-static-boolean-field object (Field->field-id Field/instance))
		      (get-boolean-field        object (Field->field-id Field/instance))))
		 (else
		  (print (format "): -- Field has unkown primitive type ~A -- :(" (to-string Field.type/Class)))(exit -1)))
	       (if (static? Field.modifiers/int)
		   (get-static-object-field     object (Field->field-id Field/instance))
		   (get-object-field            object (Field->field-id Field/instance))))))
      (delete-local-ref Field/instance)
      (delete-local-ref Field.type/Class)
      return-value)))


(define (set-field! object field-name value)
  (let* ((Field/instance       (reflected-field object    field-name))
	 (Field.type/Class     (reflected-field-type      Field/instance))
	 (Field.modifiers/int  (reflected-field-modifiers Field/instance)))

    (let ((return-value
	   (if (primitive? Field.type/Class)
	       (case (string->symbol (to-string Field.type/Class))
		 ((byte)
		  (if (static? Field.modifiers/int)
		      (set-static-byte-field    object (Field->field-id Field/instance) value)
		      (set-byte-field           object (Field->field-id Field/instance) value)))
		 ((short)
		  (if (static? Field.modifiers/int)
		      (set-static-short-field   object (Field->field-id Field/instance) value)
		      (set-short-field          object (Field->field-id Field/instance) value) ))
		 ((int)
		  (if (static? Field.modifiers/int)
		      (set-static-int-field     object (Field->field-id Field/instance) value)
		      (set-int-field            object (Field->field-id Field/instance) value)))
		 ((long)
		  (if (static? Field.modifiers/int)
		      (set-static-long-field    object (Field->field-id Field/instance) value)
		      (set-long-field           object (Field->field-id Field/instance) value)))
		 ((float)
		  (if (static? Field.modifiers/int)
		      (set-static-float-field   object (Field->field-id Field/instance) value)
		      (set-float-field          object (Field->field-id Field/instance) value)))
		 ((double)
		  (if (static? Field.modifiers/int)
		      (set-static-double-field  object (Field->field-id Field/instance) value)
		      (set-double-field         object (Field->field-id Field/instance) value)))
		 ((char)
		  (if (static? Field.modifiers/int)
		      (set-static-char-field    object (Field->field-id Field/instance) value)
		      (set-char-field           object (Field->field-id Field/instance) value)))
		 ((boolean)
		  (if (static? Field.modifiers/int)
		      (set-static-boolean-field object (Field->field-id Field/instance) value)
		      (set-boolean-field        object (Field->field-id Field/instance) value)))
		 (else
		  (print (format "): -- Field has unkown primitive type ~A -- :(" (to-string Field.type/Class)))(exit -1)))
	       (if (static? Field.modifiers/int)
		   (set-static-object-field     object (Field->field-id Field/instance) value)
		   (set-object-field            object (Field->field-id Field/instance) value)))))
      (delete-local-ref Field/instance)
      (delete-local-ref Field.type/Class)
      return-value)))

(define to-string
  (lambda (object)
    (let* ((Object.toString/method (method java.lang.Object java.lang.String toString))
	   (String/instance (call-object-method object Object.toString/method null-args))
	   (string (jstring->string String/instance)))
      (delete-local-ref String/instance) string)))

(define (jprint object)
  (print (to-string object)))

)
