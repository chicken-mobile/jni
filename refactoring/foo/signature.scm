(define (expand-type type #!optional return-type)
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

(define (mangle-class-name name)
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

(define-syntax type-signature
  (ir-macro-transformer
   (lambda (x i c)
     (let ((type (cadr x))
	   (return-type (and (pair? (cddr x)) (caddr x))))
       (expand-type type return-type)))))
