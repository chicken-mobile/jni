#>
#include <jni.h>
<#

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
 resolve-method
 method
 static-method
 constructor
 array-length
 array-ref
 list->array
 jstring
 jstring->string
 call
 get-method-modifiers
 get-method-return-type
 get-class-name
 from-reflected-method
 java-vm
 with-jvm-thread
 jprint)

(import chicken scheme foreign)
(import-for-syntax chicken data-structures)
(use lolevel)

(define-foreign-type java-vm (c-pointer "JavaVM"))
(define-foreign-type jni-env (c-pointer "JNIEnv"))
(define-foreign-type jint int)
(define-foreign-type jobject (c-pointer (struct "_jobject")))
(define-foreign-type jclass jobject)
(define-foreign-type jstring jobject)
(define-foreign-type jmethod-id (c-pointer (struct "_jmethodID")))
(define-foreign-type jfield-id (c-pointer (struct "_jfieldID")))
(define-foreign-type jsize jint)
(define-foreign-type jarray jobject)
(define-foreign-type jobject-array jarray)
(define-foreign-type jvalue (c-pointer (union "jvalue")))
(define-foreign-type jboolean bool)
(define-foreign-type jbyte char)
(define-foreign-type jchar unsigned-short char->integer integer->char)
(define-foreign-type jshort short)
(define-foreign-type jlong integer64)
(define-foreign-type jfloat float)
(define-foreign-type jdouble double)

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
  (er-macro-transformer
   (lambda (x r c)
     (let* ((return    (cadr x))
            (name      (symbol->string (caddr x)))
            (name-sym  (caddr x))
            (arg-types (cdddr x))
            (arg-names (map (lambda (i)
                              (string-append "a" (number->string i)))
                            (iota (length arg-types))))
            (arg-syms  (map string->symbol arg-names))
            (args      (map list arg-types arg-syms)))
       `(,(r 'let)
         ((,name-sym (,(r 'foreign-lambda*) ,return ((jni-env env) . ,args)
                      ,(string-append
                        (if (c return 'void)
                            "(*env)->"
                            "C_return((*env)->") name "("
                        (string-intersperse (cons "env" arg-names) ", ")
                        (if (c return 'void)
                            ");"
                            "));")))))
         (,(r 'lambda) ,arg-syms (,name-sym (,(r 'jni-env)) . ,arg-syms)))))))


(define-for-syntax call-method-max-args 5)

(define-for-syntax jni-types '(Void Object Boolean Byte Char Short Int Long Float Double)) ;; void and object must be first. cdr of this list is used when declaring field types
(define-for-syntax jni-jtypes '(void jobject jboolean jbyte jchar jshort jint jlong jfloat jdouble)) ;; void and object must be first. cdr of this list is used when declaring field types
(define-for-syntax jni-type-sigs '(Z B C S I J F D)) ;; types are corresponding jni-types

(define-syntax define-call-method-procs
  (er-macro-transformer
   (lambda (x r c)
     (let ((%begin (r 'begin))
           (%export (r 'export))
           (%define (r 'define))
           (%apply (r 'apply))
           (%jni-env-lambda (r 'jni-env-lambda)))
       (cons %begin
             (let loop ((n 0) (args '()))
               (if (= n call-method-max-args)
                   '()
                   (cons
                    (cons %begin
                          (map (lambda (return-type type)
                                 (let* ((n (number->string n))
                                        (stype (string-downcase type))
                                        (method (string->symbol (string-append "call-" stype "-method/" n)))
                                        (static-method (string->symbol (string-append "call-static-" stype "-method/" n))))
                                   `(,%begin
                                     (,%export ,method)
                                     (,%define
                                      (,method . args)
                                      (,%apply
                                       (,%jni-env-lambda ,return-type
                                                         ,(string->symbol (string-append "Call" type "Method"))
                                                         jobject
                                                         jmethod-id
                                                         . ,args)
                                       args))
                                     (,%export ,static-method)
                                     (,%define
                                      ,static-method
                                      (,%jni-env-lambda ,return-type
                                                        ,(string->symbol (string-append "CallStatic" type "Method"))
                                                        jclass
                                                        jmethod-id
                                                        . ,args)))))
                               jni-jtypes
                               (map symbol->string jni-types)))
                    (loop (+ n 1)
                          (cons 'jvalue args))))))))))

(define-call-method-procs)

(define get-field
  (jni-env-lambda jfield-id GetFieldID jclass (const c-string) (const c-string)))

;; (define get-int-field (jni-env-lambda GetIntField jint jobject jfield-id))
;; (define get-static-int-field (jni-env-lambda GetStaticIntField jint jobject jfield-id))
;; (define int-field/accessor ...)
(define-syntax define-get-field-procs
  (er-macro-transformer
    (lambda (x r c)
      (let ((%begin (r 'begin))
            (%export (r 'export))
            (%define (r 'define))
            (%apply (r 'apply))
            (%lambda (r 'lambda))
            (%car (r 'car))
            (%jvoid (r 'void))
            (%jni-env-lambda (r 'jni-env-lambda)))
        (cons %begin
              (map (lambda (return-type type type-sig)
                     (let ((proc-get-name (string->symbol (string-append "get-" (string-downcase type) "-field")))
                           (proc-set-name (string->symbol (string-append "set-" (string-downcase type) "-field")))
                           (static-proc-get-name (string->symbol (string-append "get-static-" (string-downcase type) "-field")))
                           (accessor-name (string->symbol (string-append (string-downcase type) "-field/accessor")))
                           (jni-get-name (string->symbol (string-append "Get" type "Field")))
                           (jni-set-name (string->symbol (string-append "Set" type "Field")))
                           (static-proc-set-name (string->symbol (string-append "set-static-" (string-downcase type) "-field")))
                           (static-jni-get-name (string->symbol (string-append "GetStatic" type "Field")))
                           (static-jni-set-name (string->symbol (string-append "SetStatic" type "Field")))
                           )
                       `(,%begin
                          (,%export ,static-proc-get-name)
                          (,%define ,static-proc-get-name
                                    (,%jni-env-lambda ,return-type
                                                      ,static-jni-get-name
                                                      jobject
                                                      jfield-id))
                          (,%export ,static-proc-set-name)
                          (,%define ,static-proc-set-name
                                    (,%jni-env-lambda ,%jvoid
                                                      ,static-jni-set-name
                                                      jobject
                                                      jfield-id
                                                      ,return-type))
                          (,%export ,proc-get-name)
                          (,%define ,proc-get-name
                                    (,%jni-env-lambda ,return-type
                                                      ,jni-get-name
                                                      jobject
                                                      jfield-id))
                          (,%export ,proc-set-name)
                          (,%define ,proc-set-name
                                    (,%jni-env-lambda ,%jvoid
                                                      ,jni-set-name
                                                      jobject
                                                      jfield-id
                                                      ,return-type))
                          (,%export ,accessor-name)
                          (,%define (,accessor-name object field-name)
                                    (let* ((object-class (get-object-class object))
                                           (field-id (get-field object-class field-name ,type-sig)))
                                      (,%lambda value
                                                (if (null? value)
                                                  (,proc-get-name object field-id)
                                                  (,proc-set-name object field-id (,%car value))))))
                          )))
                   (cddr jni-jtypes)
                   (map symbol->string (cddr jni-types))
                   (map symbol->string jni-type-sigs)))))))
(define-get-field-procs)

(define find-class
  (jni-env-lambda jclass FindClass (const c-string)))

(define get-object-class
  (jni-env-lambda jclass GetObjectClass jobject))

(define get-method-id
  (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string)))

(define get-static-method-id
  (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string)))

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

(define from-reflected-method
  (jni-env-lambda jmethod-id FromReflectedMethod jobject))

(define new-object
  (jni-env-lambda jobject NewObject jclass jmethod-id))

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

(define (get-method-modifiers rmethod)
  (call-int-method/0 rmethod (method java.lang.reflect.Method int getModifiers)))

(define (get-method-return-type rmethod)
  (call-object-method/0 rmethod (method java.lang.reflect.Method java.lang.Class getReturnType)))

(define (get-class-name class)
  (call-object-method/0 class (method java.lang.Class java.lang.String getName)))




(define public-modifier        1)
(define private-modifier       2)
(define protected-modifier     4)
(define static-modifier        8)
(define final-modifier        16)
(define synchronized-modifier 32)
(define volatile-modifier     64)
(define transient-modifier   128)
(define native-modifier      256)
(define interface-modifier   512)
(define abstract-modifier   1024)
(define strict-modifier     2048)

(define (abstract? modifier)
  (> (bitwise-and modifier abstract-modifier) 0))

(define (final? modifier)
  (> (bitwise-and modifier final-modifier) 0))

(define (interface? modifier)
  (> (bitwise-and modifier interface-modifier) 0))

(define (native? modifier)
  (> (bitwise-and modifier native-modifier) 0))

(define (private? modifier)
  (> (bitwise-and modifier private-modifier) 0))

(define (protected? modifier)
  (> (bitwise-and modifier protected-modifier) 0))

(define (public? modifier)
  (> (bitwise-and modifier public-modifier) 0))

(define (static? modifier)
  (> (bitwise-and modifier static-modifier) 0))

(define (strict? modifier)
  (> (bitwise-and modifier strict-modifier) 0))

(define (synchronized? modifier)
  (> (bitwise-and modifier synchronized-modifier) 0))

(define (transient? modifier)
  (> (bitwise-and modifier transient-modifier) 0))

(define (volatile? modifier)
  (> (bitwise-and modifier volatile-modifier) 0))

(define (resolve-method object method-name args)
  (let* ((rmethod (call-object-method/2
                   (get-object-class object)
                   (method java.lang.Class
                           java.lang.reflect.Method
                           getMethod
                           java.lang.String
                           #(java.lang.Class))
                   (jstring (symbol->string method-name))
                   (list->array (class java.lang.Class)
                                (map get-object-class args))))
         (method (from-reflected-method rmethod))
         (modifiers (get-method-modifiers rmethod))
         (type (string->symbol
                (jstring->string
                 (get-class-name
                  (get-method-return-type rmethod))))))
    (values type method (static? modifiers))))

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

(define-syntax call
  (ir-macro-transformer
   (lambda (x i c)
     (let* ((obj (cadr x))
            (method-name (caddr x))
            (args (cdddr x))
            (nargs (length args)))
       `(let ((obj ,obj)
              (args (list . ,args)))
          (receive (type method static?)
              (resolve-method obj ',(i method-name) args)
            (case type
              ,@(map (lambda (type)
                       `((,(i type)) ,(dispatch-method-call type nargs)))
                     '(void boolean byte char short int long float double))
              (else
               ,(dispatch-method-call 'object nargs)))))))))

(define jvm-attach-current-thread
  (foreign-lambda* int ((java-vm jvm)
			((c-pointer jni-env) env))
    "C_return((*jvm)->AttachCurrentThread(jvm, env, NULL));"))
(define jvm-detach-current-thread
  (foreign-lambda* int ((java-vm jvm))
    "C_return((*jvm)->DetachCurrentThread(jvm));"))

(define java-vm
  (make-parameter '()))

(define (with-jvm-thread jvm proc)
  (let-location ((env jni-env))
    (jvm-attach-current-thread jvm (location env))
    (parameterize ((jni-env env) (java-vm jvm)) (proc))
    (jvm-detach-current-thread jvm)))



;; needs rewrite :)
(define static-field
  (jni-env-lambda jfield-id GetStaticFieldID jclass c-string c-string))
(define static-object-field
  (jni-env-lambda jobject GetStaticObjectField jclass jfield-id))

(define (java-out)
  (let ((System (class java.lang.System)))
    (static-object-field System
     (static-field System "out" (type-signature java.io.PrintStream)))))

(define (jprint object)
  (call-void-method/1 (java-out)
			(method java.io.PrintStream 
				void 
				println 
				java.lang.Object) 
			object))


)
