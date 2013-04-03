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
          (map class->type (array->list (Method.getParameterTypes Method))))

        (define-for-syntax (find-methods class-name method-name)
          (let* ((class-object (find-class/or-error class-name))
                 (name         (symbol->string method-name))
                 (Methods      (array->list (find-methods/helper class-object name))))
            (if (not (null? Methods))
              `(jlambda-methods ',(map find-Method-parameter-types Methods))
              #f)))

        (define-for-syntax (get-field-type Field)
          (let* ((type     (Field.getType Field))
                 (str-type (to-string type)))
            (if (string-contains str-type "class")
              (class->type type)
              (string->symbol str-type))))

        (define-for-syntax (find-field class-name field-name)
          (let* ((class-object (find-class/or-error class-name))
                 (name         (symbol->string field-name))
                 (Field        (find-field/helper class-object name)))
            (if Field
              (let* ((static (static? (Field.getModifiers Field)))
                     (type   (get-field-type Field)))
                `(jlambda-field ,static ,type ,class-name ,field-name))
              #f)))

        (define-syntax jlambda 
          (ir-macro-transformer
            (lambda (x i c)
              (let* ((class-name  (strip-syntax (cadr x)))
                     (rest        (cddr x)))
                (if (null? rest)
                  `(find-class/or-error ',class-name)
                  (let ((method/field (strip-syntax (car rest))))
                    (or (find-field class-name method/field)
                        (find-methods class-name method/field)
                        (error 'jlambda "invalid jlambda expression" x )))))))))
