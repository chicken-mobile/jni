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
    (let ((object-class (get-object-class value)))
      (list value
            (to-string object-class)
            (lambda (type) (match-classes type object-class))))
    (match-value value jtypes)))

;; arg-type is list of (pointer-value to-string-of-class (lambda to match classes))
;; or list of ( ("byte" #t) ("int" #t #t))
;;
(define (match-type declared-type arg-type)
  (printf "match-type ~s ~s~n" declared-type arg-type)
  (or 
    ;; value is pointer and is instance of declared type
    (and 
        (pointer? (car arg-type))
        (call declared-type 'isInstance (car arg-type)))
    ;; or value is not pointer, so declared type must be primitive
    (and 
      (call declared-type 'isPrimitive)
      (assoc (to-string (call declared-type 'getSimpleName) arg-type)))))

(define (match-method method-types arg-types)
  (printf "match-method ~s ~s~n" method-types arg-types)
  (map (lambda (method-type arg-type)
         (match-type method-type arg-type))
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
  (map get-declared-types methods))

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
(define m-type (car m-types))
(define o-obj (call o-class 'newInstance))
(define args (list "abc" o-obj))
(define arg-types (map value->type args))

