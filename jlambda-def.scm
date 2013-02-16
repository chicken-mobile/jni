(use jni lolevel expand-full moremacros jni-reflection)

(define android-sdk-path "/home/paul/opt/adt-bundle-linux-x86/sdk")
(define android-platform-version 14)
(define android-platform-path 
  (string-append android-sdk-path "platforms/android-" (number->string android-platform-version) "/"))
(define android-platform-jar-path
  (string-append android-platform-path "android.jar"))

;;(jvm-init android-platform-jar-path)
;(jvm-init)

(import-for-syntax jni)
(define (exc wut)
  (if (exception-check)
    (begin
      (printf "~s~n" wut)
      (exception-describe)
      (exception-clear))))

(define-for-syntax (jvm-init-macros)
                   (jvm-init))
(jvm-init-macros)

(define-syntax %
  (syntax-rules ()
                ((_ arg body)
                 (lambda (arg)
                   body))))
(define id (% v v))

(define to-jbyte id)
(define to-jint id)
(define to-jlong id)
(define to-jfloat id)
(define to-jdouble id)
(define to-jstring (% value (jstring value)))
(define to-jboolean id)
(define to-jchar id)

(define (is-num? guard)
  (lambda (v)
    (and (number? v) (guard v))))

(define (make-decl-type class)
  (list (to-string class) class (get-object-class class)))

(define (decl-type-class decl-type)
  (caddr decl-type))

(define (decl-type-type decl-type)
  (cadr decl-type))

(define (prim-type-matcher names)
  (lambda (decl-type)
  (let ((decl-class (decl-type-class decl-type)))
    (if (call decl-class 'isPrimitive)
      (let ((simple-name (call decl-class 'getSimpleName)))
        (assoc names simple-name))))))

(define (make-type type-name value-matcher conv matcher)
  (list type-name value-matcher conv matcher))

(define (make-prim-type* type-name value-matcher conv . prim-type-names)
  (make-type type-name value-matcher conv (prim-type-matcher prim-type-names)))
(define (type->matcher type)
  (cadr type))
(define (type->type-matcher type)
  (cadddr type))
(define primitive-types
  (list
    (make-prim-type* "numeric" number? id "byte" "int" "long" "short" "float" "double")
;    (make-prim-type* "byte" (% v (and ((is-num? exact?) v) (> v 0) (< v 256))) to-jbyte "byte" "int" "long" "short" "float" "double")
;    (make-prim-type* "int" (is-num? exact?) to-jint "int" "long" "short" "float" "double")
    (make-type "string" string? jstring 
                    (let ((jlstring (find-class "java/lang/String")))
                      (lambda (decl-type)
                        (call (decl-type-type decl-type) 'equals jlstring))))
    ))

(define (prim-value->type value)
  (if (not (pointer? value))
    (find (% type ((type->matcher type) value))
            primitive-types)))
(define (pointer-value->type value)
  (if (pointer? value)
    (make-type (to-string value)
               #f
               id
               (lambda (decl-type)
                 (call (decl-type-type decl-type) 'isInstance value)))))
(define (match-type-to-decl-type type decl-type)
  (printf "Matching type ~s to ~s ~n" type decl-type)
  (let ((matcher (type->type-matcher type)))
    (matcher decl-type)))

;; match single method declaration with arg-types
(define (match-method-types arg-types method-types)
  (let ((matched (map (lambda (arg-type decl-type)
                 (if (match-type-to-decl-type arg-type decl-type)
                   decl-type
                   #f
                   ))
               arg-types method-types)))
    (if (member #f matched)
      #f
      matched)))

;; match all method declarations with arg-types
(define (match-methods-types arg-types methods-types)
  (filter id (map (% method-types (match-method-types arg-types method-types)) methods-types)))

(define (is-superclass? class-super class-child)
  (let loop ((super-classes (get-class-hierarchy class-child)))
    (if (null? super-classes)
      #f
      (let ((super-class (car super-classes)))
        (if (call super-class 'equals class-super)
          #t
          (loop (cdr super-classes)))))))

;; determines if decl-type1 is better then decl-type2
;; the type1 is better of type2 if type2 is super-class of type1, so it is more precise
(define (compare-decl-type decl-type1 decl-type2)
  (let ((decl-type1-class (decl-type-type decl-type1))
        (decl-type2-class (decl-type-type decl-type2)))
    (is-superclass? decl-type2-class decl-type1-class)))

;; match1 is better match2 when at least one type in match1 is subclass of match2
(define (compare-matches match1 match2)
  (if (null? match2) 
    #t
    (let loop ((match1 match1)
               (match2 match2))
      (if (or (null? match1) (not match1))
        #f
        (let ((decl-type1 (car match1))
              (decl-type2 (car match2)))
          (if (compare-decl-type decl-type1 decl-type2)
            #t
            (loop (cdr match1) (cdr match2))))))))


(define (match-method arg-types methods)
  (let loop ((methods (get-decl-method-types (get-method-types methods)))
             (best-match '()))
    (if (null? methods)
      best-match
      (let* ((method-desc (car methods))
             (foo (printf "method-desc: ~s~n" method-desc))
             (decl-types (cadr method-desc))
             (foo (printf "decl ~s~n" decl-types))
             (matched (match-method-types arg-types decl-types))
             )
        (printf "match-method: ~s~n" method-desc)
        (if (or (null? best-match)
                (compare-matches matched (cdr best-match)))
          (loop (cdr methods) (cons (car method-desc) matched))
          (loop (cdr methods) best-match))))))


;(define valueOf (jlambdas "java/lang/String" "valueOf"))
(define jtypes
  `(("byte" ((,(% v (and ((is-num? exact?) v) (> v 0) (< v 256))))
             ,to-jbyte))
    ("int" ((,(is-num? exact?)  ,number?)
            ,to-jint))
    ("long" ((,(is-num? exact?) ,number?)
             ,to-jlong))
    ("float" ((,(is-num? inexact?) ,number?)
              ,to-jfloat))
    ("double" ((,(is-num? inexact?) ,number?)
               ,to-jdouble))
    ("class java.lang.String" ((,string?) ,to-jstring))
    ("boolean" ((,boolean?) ,to-jboolean))
    ("char" ((,char? ,string?) ,to-jchar))))

;;
(define (jtype->java-type jtype)
    (car jtype))
(define (jtype->matchers jtype)
  (caadr jtype))
(define (jtype->conv jtype)
  (cadadr jtype))
(define (jtype->scheme-type jtype)
  (jtype->matchers (assoc jtype jtypes)))
(define (get-matchers type-lambdas)
  (map
    (lambda (type-lambda)
      (let ((types (car type-lambda)))
        (cons
          (map jtype->scheme-type types)
          (cdr type-lambda))))
    type-lambdas))

(define (match-types types value)
  (map (lambda (type) (type value)) types))
(define (match-args arg-types args)
  (map match-types
       arg-types
       args))

(define (scheme-type value)
  (cond
    ((string? value) 'string)
    ((exact? value) 
     (cond
       ((and (> 0 value) (< 256 value) 'byte))
       (else 'int)))
    ((inexact? value) 'double)
    ((char? value) 'char)
    ((symbol? value) 'symbol)
    
    ))

;(define (match-lambdas lambdas args)
;  (let ((arg-types (map car lambdas)))
    
;; type-lambda is a list of lists 
;; ( 
;;  (("int" "float") lambda)
;;  (....) lambda
;; )

(define (type-matcher type-lambdas)
  (let ((matchers
          (map
            (lambda (type-lambda)
              (let ((types (car type-lambda)))
                (cons
                  (map jtype->scheme-type types)
                  (cdr (type-lambda)))))
            type-lambdas)))
    (lambda args
      (let loop ((args args))
        (let ((arg (car args))
              )
          arg)))))

(define (get-methods class-name)
  (let* ((class (find-class (if (symbol? class-name) (mangle-class-name class-name) class-name)))
         (getMethods (get-method-id (get-object-class class) "getMethods" "()[Ljava/lang/reflect/Method;")))
    (array->list (call-object-method class getMethods #f))))

(define (method->name method-class)
  (let* ((getName (get-method-id (get-object-class method-class) "getName" "()Ljava/lang/String;")))
    (jstring->string (call-object-method method-class getName #f))))

;(define (methods-by-name class-name method-name)
;  (let* ((methods (get-methods class-name)))
;    (filter (lambda (method)
;              (string=? method-name (method->name method)))
;            methods)))

(define (match-1 value jtype)
  (let ((matchers (jtype->matchers jtype)))
    (filter id (map (% matcher (matcher value)) matchers))))
(define (match-value value types)
  (filter (% v (not (null? (cadr v))))
          (map (% jtype
                  (let ((matched (match-1 value jtype)))
                    (list (jtype->java-type jtype)
                          matched
                          (if (not (null? matched)) 
                            ((jtype->conv jtype) value)
                            #f))))
               types)))
(define class-name "java/lang/String")
(define class (find-class "java/lang/Object"))
(define get-class/m (get-method-id class "getClass" "()Ljava/lang/Class;"))
(define (get-class object) (call-object-method object get-class/m #f))
(define is-java/lang/object?
  (let ((j/l/o (find-class "java/lang/Object")))
    (lambda (class)
      (call j/l/o 'equals class))))

(define (is-top-hier class)
  (or (not class)
       (is-java/lang/object? class)))

(define (get-super-classes class)
  ;(printf "Get superclass of ~s~n" (to-string class))
  (let loop ((super-classes '())
             (class class))
    (if (is-top-hier class)
      (begin
   ;     (printf "SC: ~s~n" super-classes)
        super-classes)
      (let ((super-class (call class 'getSuperclass)))
        (if super-class
          (loop (cons super-class super-classes) super-class)
          super-classes)))))

(define (get-class-hierarchy class)
  (let* ((clazz (get-object-class class))
         (interfaces-m (get-method-id clazz "getInterfaces" "()[Ljava/lang/Class;"))
         (interfaces (array->list (call-object-method class interfaces-m #f)))
         (super-classes (get-super-classes class)))
    ;(printf "Interfaces: ~s~n" interfaces)
    (let loop ((hier super-classes)
               (interfaces interfaces))
      (if (null? interfaces)
        hier
        (let* ((interface (car interfaces))
               (super-interfaces (get-super-classes interface))
               )
          (printf "SI: ~s~n" super-interfaces)
          (loop (append hier (if super-interfaces
                               (append (list interface) super-interfaces)
                               (list interface)))
                (cdr interfaces)))))))


(define-for-syntax (get-ver)
                   (version))

(define (match-class arg-class-name value-class)
 (string=? arg-class-name (to-string value-class)))

(define (match-classes arg-class value-class)
  (let ((arg-class-name (to-string arg-class))
        (value-class-name (to-string value-class)))
    (if (string=? arg-class-name value-class-name)
      #t
      (let loop ((value-hierarchy (get-super-classes value-class)))
        (if (null? value-hierarchy)
          #f
          (let ((super-class (car value-hierarchy)))
            (if (match-class arg-class-name super-class)
              #t
              (loop (cdr value-hierarchy)))))))))

(define (value->type value)
  (if (pointer? value)
    (pointer-value->type value)
    (prim-value->type value)))

;    (let ((object-class (get-object-class value)))
;      (list value
;            (to-string object-class)
;            (lambda (type) (match-classes type object-class))))
;    (match-value value jtypes)))


;; arg-type is list of (pointer-value to-string-of-class (lambda to match classes))
;; or list of ( ("byte" #t) ("int" #t #t))

(define (match-type declared-type arg-type)
  (printf "match-type ~s ~s~n" declared-type arg-type)
  (let ((matched-type 
          (or 
            ;; value is pointer and is instance of declared type
            (and 
              (pointer? (car arg-type))
              (call declared-type 'isInstance (car arg-type)))
            ;; or value is not pointer, so declared type must be primitive
            (and 
              (not (pointer? (car arg-type)))
              (call declared-type 'isPrimitive)
              (let ((simple-name (call declared-type 'getSimpleName)))
                (printf "Simple-name: ~s in ~s~n" simple-name arg-type)
                (assoc (to-string simple-name) arg-type))))))
    (if matched-type arg-type (format "~s not matched ~s" declared-type arg-type ))))


(define (match-method2 method-types arg-types)
  (printf "match-method ~s ~s~n" method-types arg-types)
  (map (lambda (method-type arg-type)
         (if (match-type method-type arg-type)
           method-type
           #f
           ))
       method-types
       arg-types))

(define (match-arguments method-types args)
  (let ((arg-types (map value->type args)))
    (match-arguments* method-types arg-types)))

(define (match-arguments* methods-types arg-types)
  (let loop ((methods-types methods-types)
             (matched-method #f))
    (let* ((method-types (car methods-types)))
      (if (match-method method-types arg-types)
        (or (not matched-method)
            (compare-matches matched-method))
        1))))

(define (get-declared-types method)
  (method-parameters method))
(define (get-method-types methods)
  (map (% method (list method (get-declared-types method))) methods))
(define (get-decl-method-types methods-desc)
  (map (% method-desc (list (car method-desc) (map make-decl-type (cadr method-desc)))) methods-desc))
(define (methods-by-name/arn object method-name arn)
  (let ((methods (methods-by-name object method-name)))
    (filter (% method (let ((method-arn (length (method-parameters method))))
                        (= method-arn arn)))
            methods)))

(define o-class (find-class "o"))
(define o2-class (find-class "o$o2"))
(define mtests (methods-by-name/arn o-class 'mtest 2))
(define m1 (car mtests))
(define m1t (method-parameters m1))
(define m-types (get-method-types mtests))
;(define m-type (car m-types))
(define o-obj (call o-class 'newInstance))
(define args (list "abc" o-obj))
(define arg-types (map value->type args))
;(define m-type1 (make-decl-type (car m-type)))
;(define decl-types  (map make-decl-type m-type))
(define a1 (jstring "abc"))
(define a-type1 (pointer-value->type a1))
;(define method-decl-types (map (% v (map make-decl-type v)) m-types))
