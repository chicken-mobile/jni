(module jni-callbacks
(define-callback)
(import scheme chicken)

(begin-for-syntax
 (require-library srfi-1
                  data-structures
                  fmt
                  fmt-c
                  jni-lolevel))

(import-for-syntax matchable
                   chicken
                   extras
                   srfi-1
                   data-structures
                   fmt
                   fmt-c
                   (only jni-lolevel mangle-method-name))

(define-for-syntax declare-callback
  (match-lambda* 
    ((callback-name arguments return-type args cncb-name global-names)
     (fmt #f (apply c-fun return-type callback-name
		    (cons* '((%pointer JNIEnv) env) args)
		    (append
		     (cons (c-var 'JNIEnv 'jni_env (c-cast 'JNIEnv '*env))
			   (fold (match-lambda*
				  (((type name) global-name result)
				   (if (eq? type 'jobject)
				       (cons*		  				  
					(c-var 'jobject global-name
					       `(jni_env->NewGlobalRef env ,name))
					
					`(jni_env->DeleteLocalRef env ,name) result) result))) 
				 '() arguments global-names))		     
		     `((,cncb-name ,@global-names))))))))

(define-syntax define-callback
  (er-macro-transformer
   (lambda (x i c)     
     (match (strip-syntax x)
       ((_  callback-name arguments return-type body ...)
	  (let ((arguments (cons '(jobject target) arguments)))
	    (let ((args (map 
			 (match-lambda
			  ((type name)
			   (cond ((eq? type 'bool)    `(int ,name))
				 (else `(,type ,name))))) arguments))

		  (return-type*
		   (cond ((eq? return-type 'bool) 'int)
			 (else return-type)))

		  (cncb-args (map
			 (match-lambda
			  ((type name)
			   (cond ((eq? type 'jobject) `((c-pointer (struct _jobject)) ,name))
				 (else `(,type ,name))))) arguments))

		  (global-names (map
				 (match-lambda 
				  ((type name)
				   (if (eq? type 'jobject)
				       (symbol-append name '_global) name))) arguments))
		  (jni-cb-name (mangle-method-name callback-name))
		  (cncb-name  (symbol-append (mangle-method-name callback-name) '_cncb))) 
	      (print cncb-args)

	      `(begin
		 (define-synchronous-concurrent-native-callback (,cncb-name ,@cncb-args) ,return-type ,@body)
		 (foreign-declare 
		  ,(declare-callback jni-cb-name arguments return-type* args cncb-name global-names))))))))))
)


