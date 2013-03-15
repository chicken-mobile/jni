;; macros for automatic generation of type variants procedures:
;; - define-type-procs: iterate over types an generate call-procs and jvalue-procs
;;   - define-call-procs: generate call procedures by type, ie: call-void-method
;;   - define-jvalue-procs: generate set jvalue procedures by type (ie: set-char-jvalue!)
;; - define-get-field-procs: (ie, for double:
;;                get-double-field 
;;                set-double-field
;;                double-field/accessor
;;                get-static-double-field 
;;                set-static-double-field)
;; - define-jni-modifier-procs: generate test and accesor procedures for modifiers (ie: public? / public-modifier)

(define-syntax define-call-procs 
  (er-macro-transformer
    (lambda (x r c)
      (let* ((%begin  (r 'begin))
             (%export (r 'export))
             (%define (r 'define))
             (%env-lambda (r 'jni-env-lambda))

             (type (cadr x))
             (s-type (caddr x))

             (proc-name            (string->symbol (format "call-~A-method" (string-downcase (symbol->string type)))))
             (static-proc-name     (string->symbol (format "call-static-~A-method" (string-downcase (symbol->string type)))))
             (jni-proc-name        (string->symbol (format "Call~AMethodA" type)))
             (static-jni-proc-name (string->symbol (format "CallStatic~AMethodA" type))))

        `(,%begin
           (,%export ,proc-name)
           (,%export ,static-proc-name)

           (,%define ,proc-name
                     (,%env-lambda ,s-type ,jni-proc-name jobject jmethod-id jvalue))
           (,%define ,static-proc-name
                     (,%env-lambda ,s-type ,static-jni-proc-name jobject jmethod-id jvalue)))))))

(define-syntax define-jvalue-procs
	(er-macro-transformer
		(lambda (x r c)
			(let* ((%begin  (r 'begin))
						 (%export (r 'export))
						 (%define (r 'define))
						 (%foreign-lambda (r 'foreign-lambda))

						 (type (string->symbol (string-downcase (symbol->string (cadr x)))))
						 (s-type (caddr x))
						 (type-string (symbol->string type))
						 (set-proc-name  (string->symbol (format "set-~A-jvalue!" type-string)))
						 (c-set-proc-name (string->symbol (format "set_~A_jvalue" type-string))))

				`(,%begin
					 (,%define ,set-proc-name
										 (,%foreign-lambda jvalue ,c-set-proc-name jvalue int ,s-type)))))))

(define-syntax define-type-procs
  (er-macro-transformer
    (lambda (x r c)
      (let ((%begin (r 'begin))
            (%define-call-procs (r 'define-call-procs))
            (%define-jvalue-procs (r 'define-jvalue-procs)))
        (cons %begin
              (map (lambda (type s-type)
                     `(,%begin
                        (,%define-call-procs  ,type ,s-type)
                        (,%define-jvalue-procs ,type ,s-type)))
                   (cdr types)
                   (cdr s-types)))))))

(define-syntax define-get-field-procs
  (er-macro-transformer
    (lambda (x r c)
      (let ((%begin (r 'begin))
            (%export (r 'export))
            (%define (r 'define))
            (%apply (r 'apply))
            (%lambda (r 'lambda))
            (%car (r 'car))
            (%jvoid (r 'void))
            (%jni-env-lambda (r 'jni-env-lambda)))
        (cons %begin
              (map (lambda (return-type type type-sig)
                     (let ((proc-get-name (string->symbol (string-append "get-" (string-downcase type) "-field")))
                           (proc-set-name (string->symbol (string-append "set-" (string-downcase type) "-field")))
                           (accessor-name (string->symbol (string-append (string-downcase type) "-field/accessor")))
                           (static-proc-get-name (string->symbol (string-append "get-static-" (string-downcase type) "-field")))
                           (static-proc-set-name (string->symbol (string-append "set-static-" (string-downcase type) "-field")))
                           (jni-get-name (string->symbol (string-append "Get" type "Field")))
                           (jni-set-name (string->symbol (string-append "Set" type "Field")))
                           (static-jni-get-name (string->symbol (string-append "GetStatic" type "Field")))
                           (static-jni-set-name (string->symbol (string-append "SetStatic" type "Field"))))
                       `(,%begin
                          (,%export ,static-proc-get-name)
                          (,%define ,static-proc-get-name
                                    (,%jni-env-lambda ,return-type
                                                      ,static-jni-get-name
                                                      jobject
                                                      jfield-id))
                          (,%export ,static-proc-set-name)
                          (,%define ,static-proc-set-name
                                    (,%jni-env-lambda ,%jvoid
                                                      ,static-jni-set-name
                                                      jobject
                                                      jfield-id
                                                      ,return-type))
                          (,%export ,proc-get-name)
                          (,%define ,proc-get-name
                                    (,%jni-env-lambda ,return-type
                                                      ,jni-get-name
                                                      jobject
                                                      jfield-id))
                          (,%export ,proc-set-name)
                          (,%define ,proc-set-name
                                    (,%jni-env-lambda ,%jvoid
                                                      ,jni-set-name
                                                      jobject
                                                      jfield-id
                                                      ,return-type))
                          (,%export ,accessor-name)
                          (,%define (,accessor-name object field-name)
                                    (let* ((object-class object)
                                           (field-id (get-field object-class field-name ,type-sig)))
                                      (,%lambda value
                                                (if (null? value)
                                                  (,proc-get-name object field-id)
                                                  (,proc-set-name object field-id (,%car value))))))
                          )))
                   (cdr jni-jtypes)
                   (map symbol->string (cdr jni-types))
                   (map symbol->string jni-type-sigs)))))))

(define-syntax define-jni-modifier-procs
  (er-macro-transformer
    (lambda (x i c)
      (cons 'begin
            (let loop ((accessor-defs '())
                       (test-defs     '())
                       (exports       '())
                       (modifiers     modifiers))

              (if (null? modifiers)
                (append accessor-defs test-defs exports)
                (let* ((modifier      (car modifiers))
                       (accessor-name (string->symbol (format "~A-modifier" (car modifier))))
                       (test-name     (string->symbol (format "~A?"         (car modifier)))))

                  (loop (append accessor-defs (list `(define ,accessor-name ,(cdr modifier))))
                        (append test-defs     (list `(define (,test-name modifier)
                                                       (> (bitwise-and modifier ,(cdr modifier)) 0))))
                        (append exports       (list `(export ,accessor-name)
                                                    `(export ,test-name)))
                        (cdr modifiers)))))))))
