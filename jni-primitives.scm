#>
#include "jvalue-tools.c"
<#

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

(define-for-syntax jni-types '(Void Object Boolean Byte Char Short Int Long Float Double))
(define-for-syntax jni-jtypes '(jvoid jobject jboolean jbyte jchar jshort jint jlong jfloat jdouble))
(define-for-syntax jni-type-sigs '(V L Z B C S I J F D))

(define-for-syntax type-sigs '(V     L         Z        B     C     S      I    J      F      D))
(define-for-syntax types     '(Void  Object    Boolean  Byte  Char  Short  Int  Long   Float  Double))
(define-for-syntax s-types   '(jvoid jobject   jboolean jbyte jchar jshort jint jlong  jfloat jdouble))
(define-for-syntax c-types   '(void  c-pointer bool     byte  char  short  int  long   float  double))

;; modifiers:
(define-for-syntax modifiers
  '((public       .    1)
    (private      .    2)
    (protected    .    4) 
    (static       .    8)
    (final        .   16)
    (synchronized .   32)
    (volatile     .   64)
    (transient    .  128)
    (native       .  256)
    (interface    .  512)
    (abstract     . 1024)
    (strict       . 2048)))

;; types:
(define-foreign-type java-vm (c-pointer "JavaVM"))
(define-foreign-type jni-env (c-pointer "JNIEnv"))
(define-foreign-type jint int)
(define-foreign-type jobject (c-pointer "struct _jobject"))
(define-foreign-type jclass jobject)
(define-foreign-type jstring jobject)
(define-foreign-type jmethod-id (c-pointer (struct "_jmethodID")))
(define-foreign-type jfield-id (c-pointer (struct "_jfieldID")))
(define-foreign-type jsize jint)
(define-foreign-type jarray jobject)
(define-foreign-type jobject-array jarray)
(define-foreign-type jvalue (c-pointer (union "jvalue")))
(define-foreign-type jvoid void)
(define-foreign-type jboolean bool)
(define-foreign-type jbyte char)
(define-foreign-type jchar unsigned-short char->integer integer->char)
(define-foreign-type jshort short)
(define-foreign-type jlong integer64)
(define-foreign-type jfloat float)
(define-foreign-type jdouble double)
(define-foreign-type jthrowable jobject)

(define-for-syntax (mangle-class-name name)
  (cond
   ((symbol? name)
    (case name
      ((boolean) "java/lang/Boolean")
      ((byte)    "java/lang/Byte")
      ((char)    "java/lang/Character")
      ((short)   "java/lang/Short")
      ((int)     "java/lang/Integer")
      ((long)    "java/lang/Long")
      ((float)   "java/lang/Float")
      ((double)  "java/lang/Double")
      ((void)    "java/lang/Void")
      (else (string-translate (symbol->string name) #\. #\/))))
   ((vector? name)
    (expand-type name))))

