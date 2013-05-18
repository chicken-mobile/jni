#>
#include <jni.h>
<#

(module jni
        (jlambda jimport)
        (import scheme chicken srfi-1)
        (reexport jni-lolevel)

        (import-for-syntax jni-lolevel)
        (use jni-lolevel)

        (begin-for-syntax
          (require-library jni-lolevel srfi-1))

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

        (define-for-syntax (make-parameter-list ParameterTypes)
          (map class->type (reverse (array->list ParameterTypes))))

        (define-for-syntax (method-signature Method)
          (cons* (static? (Method.getModifiers Method))
                 (class->type (Method.getReturnType Method))
                 (make-parameter-list (Method.getParameterTypes Method))))

        (define-for-syntax (constructor-signature Constructor)
          (cons* #f 'void (make-parameter-list (Constructor.getParameterTypes Constructor))))

        (define-for-syntax (define-constructors r class-name)
          (let* ((%jlambda-methods (r 'jlambda-methods)) 
                 (class-object     (find-class/or-error class-name))
                 (Constructors     (array->list (Class.getConstructors class-object)))
                 (signatures       (map constructor-signature Constructors)))
            `(,%jlambda-methods ',class-name 'new ',signatures)))

        (define-for-syntax (define-methods r class-name method-name)
          (let* ((%jlambda-methods (r 'jlambda-methods)) 
                 (class-object     (find-class/or-error class-name))
                 (Methods          (array->list (find-methods-by-name/helper class-object (symbol->string method-name)))))
            (if (not (null? Methods))
              (let* ((static     (static? (Method.getModifiers (car Methods))))
                     (signatures (map method-signature Methods)))
                `(,%jlambda-methods ',class-name ',method-name ',signatures))
              #f)))

        (define-for-syntax (define-field r class-name field-name)
          (let* ((%lambda         (r 'lambda))
                 (%catch          (r 'catch))
                 (%jlambda-field  (r 'jlambda-field))
                 (class-object    (find-class/or-error class-name))
                 (Field           (find-field/helper class-object (symbol->string field-name))))
            (if Field
              (let* ((static (static? (Field.getModifiers Field)))
                     (type   (class->type (Field.getType Field)))) 
                `(,%catch (,%lambda () 
                                    (,%jlambda-field ,static ,type ,class-name ,field-name)) #f))
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
                    (if (eq? method/field 'new)
                      (define-constructors r class-name)
                      (or (define-field    r class-name method/field)
                          (define-methods  r class-name method/field)
                          (error 'jlambda "invalid jlambda expression" x)))))))))

        (define-for-syntax (find-unique-names elements get-name)
          (delete-duplicates (map (lambda (e)
                                    (jstring->string (get-name e))) (array->list elements))))

        (define-for-syntax (make-jlambda-definitions class-name names)
          (map (lambda (field/method) 
                 (let ((name (string->symbol field/method)))
                   `(define ,name (jlambda ,class-name ,(string->symbol field/method))))) names))

        (define-for-syntax (replace-placeholder value ls)
          (map (lambda (e) 
                 (cond ((list? e)
                        (replace-placeholder value e))
                       ((eq? '<> e)
                        value)
                       (else
                         e))) ls))

        (define-syntax jimport 
          (er-macro-transformer
            (lambda (x r c)
              (let* ((%module      (r 'module))
                     (%import      (r 'import))
                     (%begin       (r 'begin))
                     (%define      (r 'define))
                     (%jlambda     (r 'jlambda))
                     (class-name   (cadr x))
                     (specifiers   (cddr x))
                     (module-name  (r class-name))
                     (class-object (find-class/or-error class-name))
                     (Methods      (find-unique-names (find-methods/helper class-object) Method.getName))
                     (Fields       (find-unique-names (find-fields/helper  class-object) Field.getName)))
                `(,%begin (,%module ,module-name
                                    *
                                    (,%import scheme chicken jni)
                                    (use jni)
                                    (,%define new (,%jlambda ,class-name new))
                                    ,@(make-jlambda-definitions class-name Methods)
                                    ,@(make-jlambda-definitions class-name Fields))
                          (,%import ,@(if (null? specifiers) 
                                        `(,module-name) 
                                        (replace-placeholder class-name specifiers))))))))

        ) ; end of jni module
