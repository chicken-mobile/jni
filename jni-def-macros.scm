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
(define-for-syntax jni-type-sigs '(Z B C S I J F D))

(define-syntax define-call-method-procs
  (er-macro-transformer
   (lambda (x i c)
     (cons 'begin
	   (let loop ((call-defs         '())
		      (jvalue-defs       '())
		      (exports           '())
		      (jtypes            (cdr jni-types)))
	     (if (null? jtypes)
		 (append call-defs jvalue-defs exports)
		 (let* ((type   (car jtypes))
			(stype  (string->symbol (string-downcase    (symbol->string type))))
			(jstype (string->symbol (string-append  "j" (symbol->string stype))))
			(jtype  (string->symbol (string-append  "j" (string-downcase (symbol->string stype)))))
			
			(call-proc-name         (string->symbol (format "call-~A-method"        stype)))
			(static-call-proc-name  (string->symbol (format "call-static-~A-method" stype)))			  
			(call-func-name         (string->symbol (format "Call~AMethodA"          type)))
			(static-call-func-name  (string->symbol (format "CallStatic~AMethodA"    type)))
			
			(jvalue-set-proc-name   (string->symbol (format "set-~A-jvalue!"        stype)))
			(jvalue-get-proc-name   (string->symbol (format "get-~A-jvalue"        stype)))
			(jvalue-set-func-name   (string->symbol (format "set_~A_jvalue"         stype)))
			(jvalue-get-func-name   (string->symbol (format "get_~A_jvalue"         stype))))

		   (loop (append call-defs 
				 (list `(define ,call-proc-name
					  (jni-env-lambda ,jstype ,call-func-name jobject jmethod-id jvalue))
				       `(define ,static-call-proc-name
					  (jni-env-lambda ,jstype ,static-call-func-name jobject jmethod-id jvalue))))
			 (if (eq? type 'Void)
			     jvalue-defs
			     (append jvalue-defs
				     (list `(define ,jvalue-set-proc-name
					      (foreign-lambda jvalue ,jvalue-set-func-name jvalue int ,jtype)))))
			 (append exports
				 (list
				  `(export ,call-proc-name)
				  `(export ,static-call-proc-name)
				  `(export ,jvalue-set-proc-name)))
			 (cdr jtypes)))))))))

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
                                    (let* ((object-class object)
                                           (field-id (get-field object-class field-name ,type-sig)))
                                      (,%lambda value
                                                (if (null? value)
                                                  (,proc-get-name object field-id)
                                                  (,proc-set-name object field-id (,%car value))))))
                          )))
                   (cdr jni-jtypes)
                   (map symbol->string (cdr jni-types))
                   (map symbol->string jni-type-sigs)))))))

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
