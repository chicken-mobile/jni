(use jni matchable jni-lolevel)
(import-for-syntax matchable jni-lolevel)

(define-syntax jlambda-method
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier class-object return-type method-name arg-types ...)
	(let ((arg-names (arg-types->arg-names* arg-types)))
	  `(let ((method (method-id ,modifier ,class-object ,return-type ,method-name ,@arg-types))
		 (jvalue-array (make-jvalue-array ,(length arg-types)))
		 (target ,class-object))
	     (lambda ,(jlambda-args (i modifier) arg-names)
	       (fill-jvalue-array jvalue-array ,arg-types ,arg-names)
	       (jlambda-call ,modifier target ,return-type method jvalue-array)))))))))


(define-syntax jlambda-call
  (ir-macro-transformer
   (lambda (x i c)
     (match x
       ((_ modifier target return-type method jvalue-array)
	(let ((%variant (call-proc-variant (strip-syntax modifier) (strip-syntax return-type))))
	  (jlambda-postcall (strip-syntax return-type) 
			    `(,%variant target ,method jvalue-array))))))))


(define primitive-types
  '(boolean byte char short int long float double) type)

(use symbol-utils)
(define (set-jvalue! type jvalue-array index value)
  (let ((%set-jvalue (symbol-value
		      (symbol-append 'set- (type->native-type type) '-jvalue!))))
    (%set-jvalue jvalue-array index value)))

(define (fill-jvalue-array jvalue-array arg-types args)
  (map (cut set-jvalue! <> jvalue-array <> <>)
       arg-types (iota (length arg-types) args)))

(define (type->native-type type)
  (or (and (member type primitve-types) type) 'object))


(define (call-proc-variant modifier return-type)
  (if (eq? 'static modifiers)
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


(define (call-proc-variant modifier return-type)
  (symbol-append
   (or (and (eq? 'nonstatic modifier) 
	    'call-) 'call-static-) (type->native-type return-type) '-method)

  (symbol-append
   (if (eq? 'nonstatic modifier)
       'call- 'call-static-) (type->native-type return-type) '-method))

(define (jlambda-postcall* return-type return-value)
  (case return-type
    ((boolean byte char short int long float double) return-value)
    (else (prepare-local-jobject return-value))))

(define (jlambda-call* modifier target return-type method jvalue-array)
  (let ((variant (call-proc-variant modifier return-type)))
    (jlambda-postcall* return-type (variant target method jvalue-array))))

(define (jlambda-method* modifier class-object return-type method-name . arg-types)
  (let ((method (method-id* modifier class-object return-type method-name arg-types))
	(jvalue-array (make-jvalue-array (length arg-types))))
    (lambda args
      (fill-jvalue-array! jvalue-array arg-types args)
      (jlambda-call* modifier class-object return-type method jvalue-array))))
