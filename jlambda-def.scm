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
(define to-jstring id)
(define to-jboolean id)
(define to-jchar id)

;(define valueOf (jlambdas "java/lang/String" "valueOf"))
(define jtypes
  `(("byte" ((,(% v (and (exact? v) (> v 0) (< v 256))))
             ,to-jbyte))
    ("int" ((,exact? ,number?)
            ,to-jint))
    ("long" ((,exact? ,number?)
             ,to-jlong))
    ("float" ((,inexact? ,number?)
              ,to-jfloat
              ))
    ("double" ((,inexact? ,number?)
               ,to-jdouble)
               )
    ("class java.lang.String" ((,string?) ,to-jstring))
    ("boolean" ((,boolean?) ,to-jboolean))
    ("char" ((,char? ,string?) ,to-jchar))))

;;
(define (jtype->java-type jtype)
    (car jtype))
(define (jtype->matchers jtype)
  (caadr jtype))
(define (jtype->conv jtype)
  (cdadr jtype))
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

(define (match-type types value)
  (map (lambda (type) (type value)) types))
(define (match-args arg-types args)
  (map match-type
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

(define (match-value value types)
  (filter (% v (cadr v))
          (map (% jtype
                  (cons (jtype->java-type jtype)
                        (map (% matcher (matcher value))
                             (jtype->matchers jtype))))
               types)))
(define class-name "java/lang/String")
(define class (find-class "java/lang/Object"))
(define get-class/m (get-method-id class "getClass" "()Ljava/lang/Class;"))
(define (get-class object) (call-object-method object get-class/m #f))
(define is-java/lang/object?
  (let ((j/l/o (find-class "java/lang/Object")))
    (lambda (class)
      (call j/l/o 'equals class))))

(define (get-class-hierarcy class)
  (let* ((clazz (get-object-class class))
         (interfaces-m (get-method-id clazz "getInterfaces" "()[Ljava/lang/Class;"))
         (interfaces (array->list (call-object-method clazz interfaces-m #f)))
         (class-m (get-method-id clazz "getSuperclass" "()Ljava/lang/Class;"))
         (super-class (call-object-method clazz class-m #f)))
    interfaces))

(define (get-super-classes class)
  (let loop ((super-classes '())
             (class class))
    (if (is-java/lang/object? class)
      super-classes
      (let ((super-class (call class 'getSuperclass)))
        (loop (cons super-class super-classes) super-class)))))

;  (define-syntax jlambdas
;    (er-macro-transformer
;      (lambda (x r c)
;        (let ((class-name (cadr x))
;              (method-name (caddr x))
;              (types (cdddr x))
;              (%map (r 'map))
;              (%define (r 'define))
;              (
;              )
;          (%map (type)

(define-for-syntax (get-ver)
                   (version))
;(define-syntax j
;  (er-macro-transformer
;    (lambda (x r c)
;      (let ((%begin (r 'begin))
;            (%let (r 'let))
;            (%def-x (r 'def-x))
;            (ver (version)))
;        (def
