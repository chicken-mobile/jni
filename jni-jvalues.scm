(module jni-jvalues
(make-jvalue-builder %make-jvalue-builder set-jvalue!)
(import chicken scheme)
(use extras srfi-1 matchable jni-lolevel)
(begin-for-syntax
 (require-library srfi-1))
(import-for-syntax matchable srfi-1)

(define (make-jvalue-builder jvalues argument-types)
  (let ((setters 
	 (map (lambda (type index)
		(case type
		  ((boolean) (cut set-boolean-jvalue! jvalues index <>))
		  ((byte)    (cut set-byte-jvalue!    jvalues index <>))   
		  ((char)    (cut set-char-jvalue!    jvalues index <>))   
		  ((short)   (cut set-short-jvalue!   jvalues index <>))  
		  ((int)     (cut set-int-jvalue!     jvalues index <>))    
		  ((long)    (cut set-long-jvalue!    jvalues index <>))   
		  ((float)   (cut set-float-jvalue!   jvalues index <>))  
		  ((double)  (cut set-double-jvalue!  jvalues index <>)) 
		  #;
		  ((java.lang.String java.lang.CharSequence)
		   (lambda (value)
		     (set-double-jvalue! jvalues index (jstring value)))) 
		  (else      (cut set-object-jvalue!  jvalues index <>))))
	      argument-types (iota (length argument-types)))))
    (lambda args (for-each apply setters args))))



(define-for-syntax (type->native-type type)
  (case type
    ((boolean byte char short int long float double) type)
    (else 'object)))
(define-for-syntax (types->native-types types)
  (map type->native-type types))

(define-syntax set-jvalue!
  (ir-macro-transformer
   (lambda (x i c)
     (match x 
       ((_ type jvalue-array index value)
	(let ((%set-jvalue! (symbol-append 'set- (strip-syntax type) '-jvalue!)))
	  `(,%set-jvalue! ,jvalue-array ,index ,value)))))))


(define-for-syntax (set-jvalue* type jvalue-array index value)
  `(set-jvalue! ,type ,jvalue-array ,index ,value))

(define-syntax %make-jvalue-builder
  (ir-macro-transformer
   (lambda (x i c)
     (match x 
       ((_ jvalue-array (argument-types ...) (argument-names ...))
	`(begin
	   ,@(map (cut set-jvalue* <> jvalue-array <> <>)
		  (map i (types->native-types (strip-syntax argument-types)))
		  (iota (length argument-types)) argument-names)))))))
)
