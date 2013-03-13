;TODO: Some advices:
;
;  - I think "type" could be confusing in this context: there is scheme types, java types, jni types..
;  maybe "arg-types" or something like that could be more clear :-)
;
;  - Normally the convention is: "a->b" named procedures are conversion procedures, not accesors.
; so, ie: "type->matcher" should be "type-matcher"
;
;  - Maybe "type" can be a record.
;

(define primitive-types-score '(("byte" 0) ("short" 10) ("int" 20) ("long" 30) ("float" 40) ("double" 50)))

(define (prim-type-matcher names)
  (lambda (decl-type)
    (let ((decl-class (decl-type-type decl-type)))
      (if (call decl-class 'isPrimitive)
        (let ((simple-name (to-string (call decl-class 'getSimpleName))))
          ;(printf "check simple-name: ~s in ~n" simple-name names)
          (assoc simple-name names))
        (begin
          ;(printf "Not primitive: ~s~n" (to-string decl-class))
          #f)))))

(define (make-type type-name value-matcher jvalue-type matcher)
  (list type-name value-matcher jvalue-type matcher))

(define (make-prim-type* type-name value-matcher jvalue-type prim-type-names)
  (make-type type-name value-matcher jvalue-type (prim-type-matcher prim-type-names)))
(define (type->matcher type)
  (cadr type))
(define (type->type-matcher type)
  (cadddr type))
(define (type->jvalue type value)
  ((caddr type) value))

(define primitive-types
  (list
    (make-prim-type* "numeric"
                     number?
                     (lambda (v)
                       (cond ((and (<= v 127) (>= v -128)) 'byte)
                             ((and (<= v 32767) (>= v -32768)) 'short)
                             ((and (<= v 2147483647) (>= v -2147483648)) 'int) ;; whut?
                             ((not (fixnum? v)) 'float)))
                     primitive-types-score)

    (make-type "string"
               string?
               (lambda (v) 'java.lang.String)
               (let ((jlstring (find-class "java/lang/String")))
                 (lambda (decl-type)
                   (call (decl-type-type decl-type) 'equals jlstring))))))

(define (prim-value->type value)
  (assert (not (pointer? value)))
  (find (lambda (type) ((type->matcher type) value)) primitive-types))

(define (pointer-value->type value)
  (assert (pointer? value))
  (make-type (to-string value) 
             #f 
             (lambda (v) 'java.lang.Object) 
             (lambda (decl-type) 
               (call (decl-type-type decl-type) 'isInstance value))))

(define (value->type value)
  (if (pointer? value)
    (pointer-value->type value)
    (prim-value->type value)))

(define (values->types valuez)
  (map value->type valuez))
