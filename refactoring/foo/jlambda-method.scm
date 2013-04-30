(use jni-lolevel)
(import-for-syntax jni-lolevel matchable)
(include "method-id.scm")
(include "jvalue.scm")
(include "types.scm")

(define (call-proc-variant modifier return-type)
    (if (eq? modifier 'static)
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
        (else      call-static-object-method))
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
        (else      call-object-method))))

(define (jlambda-method* modifier class-object return-type method-name #!rest arg-types)
  (let ((jvalue-array     (make-jvalue-array (length arg-types))))
    (let ((jvalue-builder (make-jvalue-builder jvalue-array arg-types))
	  (call-variant   (call-proc-variant modifier return-type))
	  (method         (method-id* modifier class-object return-type method-name arg-types)))

      (if (eq? modifier 'static)
	  (lambda args
	    (jvalue-builder args)
	    (call-variant class-object method jvalue-array))
	  (lambda (target . args)
	    (jvalue-builder args)
	    (call-variant target method jvalue-array))))))



(define-for-syntax (%call-proc-variant modifier return-type)
  (symbol-append (if (eq? 'nonstatic modifier) 'call- 'call-static-)
   (type->native-type return-type) '-method))

(define-for-syntax (%arg-type->arg-name type)
  (cond ((symbol? type) (symbol-append type))
	((vector? type) (symbol-append (vector-ref type 0) 'Array))
	(else `(error "Invalid Java type signature" ,type))))
(define-for-syntax (%arg-types->arg-names arg-types)
  (map %arg-type->arg-name arg-types))

(define-for-syntax (jlambda-args modifier arg-names)
  (or (and (eq? modifier 'static) arg-names) (cons 'target arg-names)))


(define-syntax jlambda-method
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type method-name arg-types ...)
	(let ((arg-names (%arg-types->arg-names arg-types))
	      (%call-variant (%call-proc-variant (strip-syntax modifier) (strip-syntax return-type))))
	  `(let ((method (%method-id ,modifier ,class-object ,return-type ,method-name ,@arg-types))
		 (jvalue-array (make-jvalue-array ,(length arg-types)))
		 (target ,class-object))
	     (lambda ,(jlambda-args (i modifier) arg-names)
	       (%make-jvalue-builder jvalue-array ,arg-types ,arg-names)
	       (,%call-variant target method jvalue-array)))))))))



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
       (values (jlambda-method modifier class-object return-type method-name arg-type ...) ...)))))

(define-syntax jlambda-method-define
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ class-object (static-methods ...) (nonstatic-methods ...))
	`(jlambda-method-define** ,class-object
				  ,@(map (cut method-spec 'static    <>)    static-methods)
				  ,@(map (cut method-spec 'nonstatic <>) nonstatic-methods)))))))
