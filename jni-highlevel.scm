(import-for-syntax jni-lolevel)

(begin-for-syntax
  (import-for-syntax jni-lolevel chicken scheme)
  (require-library jni-lolevel))

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
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%let*                 (r 'let*))
             (%find-class/or-error  (r 'find-class/or-error))
             (%find-field           (r 'find-field))
             (%find-methods         (r 'find-methods))
             (%if                   (r 'if))
             (%null?                (r 'null?))
             (class-name            (cadr x))
             (rest                  (cddr x)))
        (if (null? rest)
           `(,%find-class/or-error ',class-name)
           (let ((method/field (car rest)))
             (or (find-field class-name method/field)
                 (find-methods class-name method/field)
                 (error 'jlambda "invalid jlambda expression"))))))))
