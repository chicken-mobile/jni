;; jni-reflection: high level java procedures

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

(define (jprint . values)
  (for-each display
            (map (lambda (value)
                   (if (pointer? value)
                     (to-string value) 
                     value))
                 (cons values "\n"))))

(define Class.isPrimitive
  (jlambda-method* #f boolean java.lang.Class isPrimitive))
(define Class.getMethods
  (jlambda-method* #f #(java.lang.reflect.Method) java.lang.Class getMethods))
(define Class.getDeclaredMethods
  (jlambda-method* #f #(java.lang.reflect.Method) java.lang.Class getDeclaredMethods))

(define Method.getModifiers
  (jlambda-method* #f int java.lang.reflect.Method getModifiers))
(define Method.getReturnType
  (jlambda-method* #f java.lang.Class java.lang.reflect.Method getReturnType))
(define Method.getName
  (jlambda-method* #f java.lang.String java.lang.reflect.Method getName))
(define Method.getParameterTypes
  (jlambda-method* #f #(java.lang.Class) java.lang.reflect.Method getParameterTypes))

(define find-field/helper
	(jlambda-method* (static) java.lang.reflect.Field 
									com.chicken_mobile.jni.ReflectionHelper findField java.lang.Class java.lang.String))

(define find-methods/helper
	(jlambda-method* (static) #(java.lang.reflect.Method)
									com.chicken_mobile.jni.ReflectionHelper findMethods java.lang.Class java.lang.String))

(define Field.getType
  (jlambda-method* #f java.lang.Class java.lang.reflect.Field getType))

(define Field.getModifiers
  (jlambda-method* #f int java.lang.reflect.Field getModifiers))

(define (get-field-type Field)
	(let* ((type     (Field.getType Field))
				 (str-type (to-string type)))
		(if (string-contains str-type "class")
			(class->type type)
			(string->symbol str-type))))

(define-syntax find-field 
  (er-macro-transformer
    (lambda (x r c)
    	(let* ((%let*            (r 'let*))
             (%class           (r 'class/or-error))
             (%if              (r 'if))
    				 (%find-field/helper  (r 'find-field/helper))
    				 (class-name       (cadr x))
    				 (field-name       (caddr x)))
    		`(,%let* ((class-object (,%class ,class-name))
    							(name         (symbol->string ,field-name))
									(Field        (,%find-field/helper class-object name)))
								 (,%if Field
											 (,%let* ((static (static? (Field.getModifiers Field)))
																(type   (get-field-type Field)))
															 (jlambda-field* static type ,class-name ,field-name))
											 #f))))))

(define (find-Method-parameter-types Method)
	(map class->type (array->list (Method.getParameterTypes Method))))

(define-syntax find-methods
  (er-macro-transformer
    (lambda (x r c)
    	(let* ((%let*            (r 'let*))
             (%class           (r 'class/or-error))
             (%if              (r 'if))
    				 (%find-methods/helper  (r 'find-methods/helper))
    				 (%array->list     (r 'array->list))
    				 (class-name       (cadr x))
    				 (method-name      (caddr x)))
    		`(,%let* ((class-object (,%class ,class-name))
									(name         (symbol->string ,method-name))
									(Methods      (,%array->list (,%find-methods/helper class-object name))))
								 (if (not (null? Methods))
									 (map find-Method-parameter-types Methods)
								 	 #f))))))

(define-syntax jlambda 
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%let*         (r 'let*))
             (%class        (r 'class/or-error))
             (%find-field   (r 'find-field))
             (%find-methods (r 'find-methods))
             (%if           (r 'if))
             (%null?        (r 'null?))
             (class-name    (cadr x))
             (rest          (cddr x)))
        `(,%let* ((class-object (,%class ,class-name))
                  (rest         ',rest))
           (,%if (,%null? rest)
             class-object
						 (,%let* ((method/field (car rest)))
						 	 (or (,%find-field ,class-name method/field)
						 	 		 (,%find-methods ,class-name method/field)
									 (error 'jlambda "invalid jlambda expression")))))))))
