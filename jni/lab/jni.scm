(module jni

(jni-init
 jni-env
 find-class
 class
 type-signature
 define-method
 get-object-class
 get-static-method-id
 get-method-id
 method
 constructor
 static-method)

(import chicken scheme foreign)
(import-for-syntax chicken data-structures)

(define-foreign-type jni-env (c-pointer "JNIEnv"))
(define-foreign-type jint int)
(define-foreign-type jobject (c-pointer (struct "_jobject")))
(define-foreign-type jclass jobject)
(define-foreign-type jstring jobject)
(define-foreign-type jmethod-id (c-pointer (struct "_jmethodID")))

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

(define-syntax jni-env-lambda
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((return    (cadr x))
            (name      (symbol->string (i (caddr x))))
            (name-sym  (caddr x))
            (arg-types (cdddr x))
            (arg-names (map (lambda (i)
                              (string-append "a" (number->string i)))
                            (iota (length arg-types))))
            (arg-syms  (map string->symbol arg-names))
            (args      (map list arg-types (i arg-syms))))
       `(let ((,name-sym (foreign-lambda* ,(i return) ((jni-env env) . ,(i args))
                           ,(string-append "C_return((*env)->" name "(env, "
                                           (string-intersperse arg-names ", ")
                                           "));"))))
          (lambda ,arg-syms (,name-sym (jni-env) . ,arg-syms)))))))

(define find-class
  (jni-env-lambda jclass FindClass (const c-string)))

(define get-object-class
  (jni-env-lambda jclass GetObjectClass jobject))

(define get-method-id
  (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string)))

(define get-static-method-id
  (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string)))

;; (define call-void-method
;;   (jni-env-lambda void CallVoidMethod jobject jmethod-id))

;; (define call-static-void-method
;;   (jni-env-lambda void CallStaticVoidMethod jobject jmethod-id))

;; (define get-utf-chars
;;   (jni-env-lambda c-string GetStringUTFChars jstring jboolean))

;; (define release-utf-chars ;;invalid pointer maybe the scheme string is other memmory ....
;;   (jni-env-lambda void ReleaseStringUTFChars jstring (const c-string)))

;; (define new-object
;;   (jni-env-lambda jobject NewObject jclass jmethod-id))

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
                          (,(i '(c-pointer (struct "_jobject"))) class)
                          . ,(cdadr x))
          ,(i (caddr x))
          (parameterize ((jni-env env))
            . ,(cdddr x)))))))

;; (define-syntax call
;;   (syntax-rules (())))

)
