#>
#include <jni.h>
<#

(module jni
        (jlambda)
        (import scheme chicken)
        (reexport jni-lolevel)

        (import-for-syntax jni-lolevel)
        (use jni-lolevel)

        (begin-for-syntax
          (import-for-syntax jni-lolevel chicken scheme)
          (require-library jni-lolevel))

        (cond-expand 
          (android)
          (else
            (export jvm-init)
            (define-syntax jvm-init
              (ir-macro-transformer
                (lambda (x i c)
                  (let ((class-path  (if (null? (cdr x)) "." (cadr x))))
                    (if (not (jni-env))
                      (jvm-init-lolevel class-path))
                    `(unless (jni-env) 
                       (jvm-init-lolevel ,class-path))))))))

        (define-for-syntax (find-Method-parameter-types Method)
          (map class->type (reverse (array->list (Method.getParameterTypes Method)))))

        (define-for-syntax (build-method-signature Method)
          (cons (class->type (Method.getReturnType Method)) (find-Method-parameter-types Method)))

        (define-for-syntax (find-methods class-name method-name)
          (let* ((class-object (find-class/or-error class-name))
                 (name         (symbol->string method-name))
                 (Methods      (array->list (find-methods/helper class-object name))))
            (if (not (null? Methods))
              (let* ((static     (static? (Method.getModifiers (car Methods))))
                     (signatures (map build-method-signature Methods)))
                `(jlambda-methods ,static ',class-name ',method-name ',signatures))
              #f)))

        (define-for-syntax (find-field class-name field-name)
          (let* ((class-object (find-class/or-error class-name))
                 (name         (symbol->string field-name))
                 (Field        (find-field/helper class-object name)))
            (if Field
              (let* ((static (static? (Field.getModifiers Field)))
                     (type   (class->type (Field.getType Field)))) 
                `(jlambda-field ,static ,type ,class-name ,field-name))
              #f)))

        (define-syntax jlambda 
          (er-macro-transformer
            (lambda (x r c)
              (let* ((%find-class/or-error (r 'find-class/or-error))
              			 (class-name           (cadr x))
                     (rest                 (cddr x)))
                (if (null? rest)
                  `(,%find-class/or-error ',class-name)
                  (let ((method/field (car rest)))
                    (or (find-field class-name method/field)
                        (find-methods class-name method/field)
                        (error 'jlambda "invalid jlambda expression" x)))))))))
