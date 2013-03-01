;; jni-java-utils.scm: high level java procedures

(define-syntax jimport
	(er-macro-transformer
		(lambda (x r c)
			(pp x)
			(let ((object-class (class* (cadr x))))
				(pp object-class)
				`(list ,@(map (lambda (method)
												(let ((class-name (string->symbol (string-drop (to-string object-class) 6)))
															(modifier (Method.getModifiers method))
															(name (Method.getName method))
															(return-type (Method.getReturnType method))
															(params (array->list* (Method.getParameterTypes method))))

													(let ((modifier-symbols
																	(let ((modifier-symbols '()))
																		(if (not (public? modifier))
																			(set! modifier-symbols (cons 'private modifier-symbols)))
																		(if (static? modifier)
																			(set! modifier-symbols (cons 'static modifier-symbols)))			     			      
																		modifier-symbols)))

														(let ((jlambda-def 
																		`(,class-name
																			 ,(string->symbol (let ((foo (to-string return-type)))
																													(if (string-prefix? "class " foo)
																														(string-drop foo 6)
																														foo)) ) 
																			 ,(string->symbol (jstring->string name)) 
																			 ,@(map (lambda (param)
																								(string->symbol (let ((foo (to-string param)))
																																	(if (string-prefix? "class " foo)
																																		(string-drop foo 6)
																																		foo))))
																							params))))
															(cons 'jlambda-method
																		(if (null? modifier-symbols)
																			(cons #f jlambda-def)
																			(cons modifier-symbols jlambda-def))))
														)))
											(array->list* (Class.getDeclaredMethods object-class))))))))

(define jstring->string
  (let ((get-chars     (jni-env-lambda (c-pointer (const char)) GetStringUTFChars jstring c-pointer))
        (release-chars (jni-env-lambda void ReleaseStringUTFChars jstring (c-pointer (const char))))
        (get-length    (jni-env-lambda jsize GetStringUTFLength jstring)))
    (lambda (jstring)
      (let* ((chars (get-chars jstring #f))
             (len   (get-length jstring))
             (str   (make-string len)))
        (move-memory! chars str len)
        (release-chars jstring chars)
        str))))

(define (array->list array-object)
  (do ((idx 0 (+ idx 1))
       (object-list '() (cons (array-ref array-object idx) object-list)))
    ((<= (array-length array-object) idx) object-list)))

(define (list->array class lst)
  (let ((arr (make-array (length lst) class #f)))
    (let loop ((i 0) (lst lst))
      (if (null? lst)
        arr
        (begin
          (array-set! arr i (car lst))
          (loop (+ i 1) (cdr lst)))))))

(define to-string
  (lambda (object)
    (let* ((Object.toString/method (method java.lang.String java.lang.Object toString))
           (String/instance (call-object-method object Object.toString/method #f))
           (string (jstring->string String/instance)))
      (delete-local-ref String/instance) string)))

(define (jprint . values)
  (for-each display
            (map (lambda (value)
                   (if (pointer? value)
                     (to-string value) 
                     value))
                 (cons values "\n"))))

(define Class.isPrimitive
  (jlambda-method #f boolean java.lang.Class isPrimitive))
(define Class.getMethods
  (jlambda-method #f #(java.lang.reflect.Method) java.lang.Class getMethods))
(define Class.getDeclaredMethods
  (jlambda-method #f #(java.lang.reflect.Method) java.lang.Class getDeclaredMethods))

(define Method.getModifiers
  (jlambda-method #f int java.lang.reflect.Method getModifiers))
(define Method.getReturnType
  (jlambda-method #f java.lang.Class java.lang.reflect.Method getReturnType))
(define Method.getName
  (jlambda-method #f java.lang.String java.lang.reflect.Method getName))
(define Method.getParameterTypes
  (jlambda-method #f #(java.lang.Class) java.lang.reflect.Method getParameterTypes))
