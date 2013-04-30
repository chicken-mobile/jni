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
  (symbol-append
   (if (eq? 'nonstatic modifier)
       'call- 'call-static-) (type->native-type return-type) '-method))

(define-for-syntax (class-or-free-class modifier class-object)
  (if (eq? 'static modifier) `(prepare-local-jobject ,class-object) class-object))

(define-for-syntax (arg-types->arg-names* arg-types)
  (map (lambda (type)
	 (cond ((symbol? type) (symbol-append type))
	       ((vector? type) (symbol-append (vector-ref type 0) 'Array))
	       (else `(error "Invalid Java type signature" ,type)))) arg-types))

(define-for-syntax (jlambda-args modifier arg-names)
  (or (and (eq? modifier 'static) arg-names) (cons 'target arg-names)))


(define-syntax jlambda-method
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type method-name arg-types ...)
	(let ((arg-names (arg-types->arg-names* arg-types))
	      (%call-variant (%call-proc-variant modifier return-type)))
	  `(let ((method (%method-id ,modifier ,class-object ,return-type ,method-name ,@arg-types))
		 (jvalue-array (make-jvalue-array ,(length arg-types)))
		 (target ,(class-or-free-class modifier class-object)))
	     (lambda ,(jlambda-args (i modifier) arg-names)
	       (%make-jvalue-builder jvalue-array ,arg-types ,arg-names)
	       (,%call-variant target method jvalue-array)))))))))


