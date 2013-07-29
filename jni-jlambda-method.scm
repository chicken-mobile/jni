(module jni-jlambda-method

(jlambda-non-overloaded-method* jlambda-non-overloaded-method jlambda-method-define)

(import chicken scheme matchable)
(use lolevel jni-lolevel jni-types jni-jvalues jni-method-id jni-jvalues jni-signatures)
(begin-for-syntax 
 (require-library jni-lolevel jni-signatures))
(import-for-syntax matchable chicken extras jni-lolevel jni-signatures)
(begin-for-syntax (attach-thread))

(attach-thread)


(define (call-proc-variant modifier return-type)
  (case modifier
    ((static)
     (case return-type
       ((void)    call-static-void-method)    
       ((boolean) call-static-boolean-method) 
       ((byte)    call-static-byte-method)    
       ((char)    call-static-char-method)    
       ((short)   call-static-short-method)   
       ((int)     call-static-int-method)     
       ((long)    call-static-long-method)    
       ((float)   call-static-float-method)   
       ((double)  call-static-double-method)
       (else      call-static-object-method)))
    ((nonstatic)
     (case return-type
        ((void)    call-void-method)    
        ((boolean) call-boolean-method) 
        ((byte)    call-byte-method)    
        ((char)    call-char-method)    
        ((short)   call-short-method)   
        ((int)     call-int-method)
        ((long)    call-long-method)   
        ((float)   call-float-method)  
        ((double)  call-double-method)
        (else      call-object-method)))
    ((constructor) new-object)))

(define (jlambda-non-overloaded-method* #!rest args)
  (match args
    (( modifier class-object return-type method-name arg-types ...)
     (let ((jvalue-array     (make-jvalue-array (length arg-types))))
       (let ((jvalue-builder (make-jvalue-builder jvalue-array arg-types))
	     (call-variant   (call-proc-variant modifier return-type))
	     (method         (method-id* modifier class-object return-type method-name arg-types)))
	 
	 (if (eq? modifier 'nonstatic)
	     (lambda (target . args)
	       (jvalue-builder args)
	       (call-variant target method jvalue-array))
	     (lambda args
	       (jvalue-builder args)
	       (call-variant class-object method jvalue-array))))))
    (('constructor class-object arg-types ...)
     (apply jlambda-non-overloaded-method* (append (list 'constructor class-object 'void "<init>") arg-types)))))


(define-for-syntax (%call-proc-variant modifier return-type)
  (let ((type (type->native-type return-type)))
    (case modifier
      ((static)      (symbol-append 'call-static- type '-method))
      ((nonstatic)   (symbol-append 'call- type '-method))
      ((constructor) 'new-object))))

(define-for-syntax (%arg-type->arg-name type)
  (let ((type (strip-syntax type)))
    (cond ((symbol? type) (symbol-append type))
	  ((vector? type) (symbol-append (vector-ref type 0) 'Array))
	  (else (error "Invalid Java type signature" type)))))
(define-for-syntax (%arg-types->arg-names arg-types)
  (fold 
   (lambda (type idx names)
     (cons (symbol-append type (string->symbol (->string idx))) names))
   '()
   (map %arg-type->arg-name arg-types)
   (reverse (iota (length arg-types)))))

(define-for-syntax (jlambda-args modifier arg-names)
  (or (and (eq? modifier 'nonstatic) (cons 'target arg-names)) arg-names))


(define-syntax jlambda-non-overloaded-method
  (ir-macro-transformer
   (lambda (x i c)
     (match (strip-syntax x)
       ((_ class-object modifier return-type method-name arg-types ...)
	(let ((arg-names (strip-syntax (%arg-types->arg-names arg-types)))
	      (%call-variant (%call-proc-variant (strip-syntax modifier) (strip-syntax return-type))))
	  `(let ((method (method-id ,modifier ,class-object ,return-type ,method-name ,@arg-types))
		 (jvalue-array (make-jvalue-array ,(length arg-types)))
		 (target ,class-object))
	     (extend-procedure
	      (lambda ,(jlambda-args (strip-syntax modifier) arg-names)
		(%make-jvalue-builder jvalue-array ,arg-types ,arg-names)
		(,%call-variant target method jvalue-array))
	      ,(if (null? arg-types)
		   `(list ,class-object ',modifier ',return-type ',method-name)
		   `(list ,class-object ',modifier ',return-type ',method-name ,@(map (lambda (x) `',x) arg-types)))))))))))



(define-for-syntax (method-spec modifier spec)
  (match spec
    ((proc-name (return-type method-name arg-types ...))
     `(,modifier ,return-type ,method-name ,@arg-types ,proc-name))
    ((return-type method-name arg-types ...)
     `(,modifier ,return-type ,method-name ,@arg-types ,method-name))))

(define-syntax jlambda-method-define*
  (syntax-rules ()
    ((_ class-object (modifier return-type method-name arg-type ... proc-name) ...)
     (define-values (proc-name ...) 
       (values (jlambda-non-overloaded-method modifier class-object return-type method-name arg-type ...) ...)))))

(define-syntax jlambda-method-define
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ class-object (static-methods ...) (nonstatic-methods ...))
	`(jlambda-method-define* ,class-object
				 ,@(map (cut method-spec 'static    <>)    static-methods)
				 ,@(map (cut method-spec 'nonstatic <>) nonstatic-methods)))))))
)
