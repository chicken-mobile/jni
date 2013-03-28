#>
#include "jvalue-tools.c"
<#

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

;;mangling
(define (mangle-class-name name)
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

(define-for-syntax (mangle-method-name name)
  (string->symbol
   (string-append "Java_" (string-translate (symbol->string name) #\. #\_))))

; jobject definition
(define-record jobject-meta)
(define (jobject? pointer)
  (and (pointer? pointer)
       (jobject-meta? (pointer-tag pointer))))
(mutate-procedure ##sys#pointer->string
  (lambda (old)
    (lambda args
			(let ((arg (car args)))
				(if (jobject-meta? (pointer-tag arg))
					(let* ((object-class (get-object-class arg))
								 (jobject-string (format "#<jref <~A> ~A>" (to-string object-class) (to-string arg))))
						(delete-local-ref object-class)
						jobject-string)
					(apply old args))))))

(define (prepare-local-jobject jobject)
	(if (pointer? jobject) ; if an exception is raised in java code, the returned type is not a jobject
		(set-finalizer! (tag-pointer jobject (make-jobject-meta)) delete-local-ref)
		jobject))

(define (prepare-local-jclass jclass)
  (set-finalizer! jclass delete-local-ref))

;; jni jvm bindings
(define-foreign-variable JNI_VERSION_1_1 int)
(define-foreign-variable JNI_VERSION_1_2 int)
(define-foreign-variable JNI_VERSION_1_4 int)
(define-foreign-variable JNI_VERSION_1_6 int)

(define jvm-destroy
  (foreign-lambda* jint ((java-vm jvm))
    "C_return((*jvm)->DestroyJavaVM(jvm));"))

(define jvm-env
  (foreign-lambda* jint ((java-vm jvm) ((c-pointer (c-pointer void)) env) (jint version))
    "C_return((*jvm)->GetEnv(jvm, env, version));"))

(define jvm-attach-current-thread
  (foreign-lambda* int ((java-vm jvm) ((c-pointer (c-pointer void)) env))
    "C_return((*jvm)->AttachCurrentThread(jvm, env, NULL));"))

(define jvm-detach-current-thread
  (foreign-lambda* int ((java-vm jvm))
    "C_return((*jvm)->DetachCurrentThread(jvm));"))

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

(define (primitive? type)
  (member type '(void boolean byte char short int long float double)))
