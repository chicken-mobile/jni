(use expand-full )
(use jni)
(import-for-syntax chicken matchable)

(define-for-syntax (mangle-class-name name)
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

(define-for-syntax (expand-type type #!optional return-type)
  (cond ((symbol? type)
	 (case type
	   ((boolean) "Z")
	   ((byte)    "B")
	   ((char)    "C")
	   ((short)   "S")
	   ((int)     "I")
	   ((long)    "J")
	   ((float)   "F")
	   ((double)  "D")
	   ((void)    "V")
	   (else (string-append "L" (mangle-class-name type) ";"))))
	((vector? type)
	 (string-append "[" (expand-type (vector-ref type 0))))
	((list? type)
	 (and-let* ((return (expand-type return-type)))
	   (string-append "(" (string-intersperse (map expand-type type) "") ")" return)))))

(define-syntax type-signature
  (ir-macro-transformer
   (lambda (x i c)
     (let ((type (i (cadr x)))
	   (return-type (and (pair? (cddr x)) (i (caddr x)))))
       (expand-type type return-type)))))

(define-syntax class
  (ir-macro-transformer
   (lambda (x i c)
     (let ((class-name-sym (cadr x)))
       `(find-class ,(mangle-class-name (i class-name-sym)))))))


(define-for-syntax (type->native-type type)
  (case type
    ((boolean byte char short int long float double) type)
    (else 'object)))
(define-for-syntax (types->native-types types)
  (map type->native-type types))

(define-syntax set-jvalue!
  (ir-macro-transformer
   (lambda (x i c)
     (match x ((_ type jvalue-array index value)
       (let ((%set-jvalue! (symbol-append 'set- (strip-syntax type) '-jvalue!)))
	 `(,%set-jvalue! ,jvalue-array ,index ,value)))))))


(define-for-syntax (set-jvalue* type jvalue-array index value)
  `(set-jvalue! ,type ,jvalue-array ,index ,value))

(define-syntax fill-jvalue-array
  (ir-macro-transformer
   (lambda (x i c)
     (match x ((_ jvalue-array (argument-types ...) (argument-names ...))
       `(begin
	  ,@(map (cut set-jvalue* <> jvalue-array <> <>)
		 (map i (types->native-types (strip-syntax argument-types)))
		 (iota (length argument-types))
		 argument-names)))))))

(define-syntax method-id
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type method-name arg-types ...)
	(let ((%method-id  (or (and (c modifier 'static) 'get-static-method-id) 'get-method-id))
	      (method-name (symbol->string (strip-syntax method-name))))
	  `(,%method-id ,class-object ,method-name (type-signature ,arg-types ,return-type))))))))

(define-syntax field-id
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type field-name)
	(let ((%field-id  (or (and (c modifier 'static) 'get-static-field) 'get-field))
	      (field-name (symbol->string (strip-syntax field-name))))
	  `(,%field-id ,class-object ,field-name (type-signature ,return-type))))))))


(define-for-syntax (call-proc-variant modifier return-type)
  (symbol-append
   (if (eq? 'nonstatic modifier)
       'call- 'call-static-) (type->native-type return-type) '-method))

(define-for-syntax (jlambda-postcall return-type call-variant)
  `(check-jexception 
    ,(case return-type
       ((boolean byte char short int long float double) call-variant)
       (else `(prepare-local-jobject ,call-variant)))))

(define-syntax jlambda-call
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier target return-type method jvalue-array)
	(let ((%variant (call-proc-variant (strip-syntax modifier) (strip-syntax return-type))))
	  (jlambda-postcall (strip-syntax return-type) 
			    `(,%variant target ,method jvalue-array))))))))

(define-for-syntax (arg-types->arg-names* arg-types)
  (map (lambda (type)
	 (cond ((symbol? type) (symbol-append type))
	       ((vector? type) (symbol-append (vector-ref type 0) 'Array))
	       (else `(error "Invalid Java type signature" ,type))))
       arg-types))

(define-for-syntax (jlambda-args modifier arg-names)
  (or (and (eq? modifier 'static) arg-names) (cons 'target arg-names)))
(define-for-syntax (class-or-free-class modifier class-object)
  (if (eq? 'static modifier)
      `(prepare-local-jobject ,class-object)
      `(prepare-local-jobject ,class-object)))


(define-syntax jlambda-method*
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type method-name arg-types ...)
	(let ((arg-names (arg-types->arg-names* arg-types)))
	  `(let ((mid (method-id ,modifier ,class-object ,return-type ,method-name ,@arg-types))
		 (jvalue-array (make-jvalue-array ,(length arg-types)))
		 (target class-object))
	     (lambda ,(jlambda-args (i modifier) arg-names)
	       (fill-jvalue-array jvalue-array ,arg-types ,arg-names)
	       (jlambda-call ,modifier target ,return-type mid jvalue-array)))))))))


(define-for-syntax (field-getter modifier type)
  (symbol-append (if (eq? modifier 'nonstatic)
		     'get- 'get-static-) type '-field))
(define-for-syntax (target-or-class modifier class-object)
  (if (eq? modifier 'nonstatic) 'target class-object))
(define-for-syntax (make-field-getter modifier class-object type field)
  `(lambda ,(jlambda-args (strip-syntax modifier) '())
     (,(field-getter modifier type) ,(target-or-class (strip-syntax modifier) class-object) ,field)))

(define-syntax jlambda-field*
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object type field-name)
	`(let ((field  (field-id ,modifier ,class-object ,type ,field-name)))
	   (getter-with-setter 
	    ,(make-field-getter (strip-syntax modifier) class-object (strip-syntax type) 'field)
	    void)))))))

(define-syntax jlambda-method
  (syntax-rules ()
    ((_ modifier class-name return-type method-name arg-type ...)
     (let ((class-object (class class-name)))
       (jlambda-method* modifier class-object return-type meta-name arg-type ...)))))

(define-syntax jlambda-method-define*
  (syntax-rules ()
    ((_ class-name (modifier return-type method-name arg-type ... proc-name) ...)
     (define-values (proc-name ...) 
       (let ((class-object (class class-name))) 
	 (values (jlambda-method* modifier class-object return-type method-name arg-type ...) ...))))))

(define-for-syntax (method-spec modifier spec)
  (match spec
    ((proc-name (return-type method-name arg-types ...))
     `(,modifier ,return-type ,method-name ,@arg-types ,proc-name))
    ((return-type method-name arg-types ...)
     `(,modifier ,return-type ,method-name ,@arg-types ,method-name))))

(define-syntax jlambda-method-define
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ class-name (static-methods ...) (nonstatic-methods ...))
	`(jlambda-method-define* ,class-name 
	   ,@(map (cut method-spec 'static    <>)    static-methods)
	   ,@(map (cut method-spec 'nonstatic <>) nonstatic-methods)))))))



(pp (expand* 
  '(jlambda-method-define java.lang.Class
     ()
     ((primitive? (boolean isPrimitive))
      (boolean isInterface)
      (boolean isLocalClass)
      (boolean isMemberClass)
      (boolean isSynthetic)
      (int                              getModifiers)
      (java.lang.Package                getPackage)
      (java.lang.String                 getName)
      (java.lang.String                 getSimpleName)
      (#(java.lang.reflect.Method)      getMethods)
      (#(java.lang.reflect.Method)      getDeclaredMethods)
      (#(java.lang.reflect.Constructor) getConstructors)
      (#(java.lang.reflect.Constructor) getDeclaredConstructors)
      (#(java.lang.reflect.Constructor) getConstructor         #(java.lang.Class))
      (#(java.lang.reflect.Constructor) getDeclaredConstructor #(java.lang.Class))
      (#(java.lang.reflect.Field)       getFields)
      (#(java.lang.reflect.Field)       getDeclaredFields)))))

(import-for-syntax jni)
(begin-for-syntax
 (require-library jni)
 (unless (jni-env) (jvm-init)))
(unless (jni-env) (jvm-init))

(jlambda-method-define java.lang.Class
  ()
  ((primitive? (boolean isPrimitive))
   (boolean isInterface)
   (boolean isLocalClass)
   (boolean isMemberClass)
   (boolean isSynthetic)
   (int                              getModifiers)
   (java.lang.Package                getPackage)
   (java.lang.String                 getName)
   (java.lang.String                 getSimpleName)
   (#(java.lang.reflect.Method)      getMethods)
   (#(java.lang.reflect.Method)      getDeclaredMethods)
   (#(java.lang.reflect.Constructor) getConstructors)
   (#(java.lang.reflect.Constructor) getDeclaredConstructors)
   (#(java.lang.reflect.Constructor) getConstructor         #(java.lang.Class))
   (#(java.lang.reflect.Constructor) getDeclaredConstructor #(java.lang.Class))
   (#(java.lang.reflect.Field)       getFields)
   (#(java.lang.reflect.Field)       getDeclaredFields)))

(ppexpand* '(jlambda-field* nonstatic class-object int MIN_VALUE))
(ppexpand* '(jlambda-field* static class-object int MIN_VALUE))


(let ((Integer (class java.lang.Integer)))
  (define Integer.MIN_VALUE
    (jlambda-field* static Integer int MIN_VALUE))
  (define Integer.MAX_VALUE
    (jlambda-field* static Integer int MAX_VALUE))

  (print (list (Integer.MIN_VALUE) (Integer.MAX_VALUE))))

