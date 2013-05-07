(define ExceptionHelper (class com.chicken_mobile.jni.ExceptionHelper))
(define Exception (class java.lang.Exception))

(define jexception-trace
  (let ((m (jlambda-non-overloaded-method static ExceptionHelper java.lang.String traceAsString java.lang.Exception)))
    (lambda (exception)
      (jstring->string! (m exception)))))

(define jexception-message 
  (let ((m (jlambda-non-overloaded-method nonstatic Exception java.lang.String getMessage)))
    (lambda (exception)
      (jstring->string! (m exception)))))

(define jexception-type
  (let ((m (jlambda-non-overloaded-method static ExceptionHelper java.lang.String type java.lang.Exception)))
    (lambda (exception)
      (jstring->string! (m exception)))))

(define (make-condition exception)
  (let ((trace   (jexception-trace exception))
        (message (jexception-message exception))
        (type    (string->symbol (jexception-type exception))))
      (exception-clear)
      (make-composite-condition
        (make-property-condition 'exn)
        (make-property-condition 'java 'trace trace 'message message 'type type)
        (make-property-condition type))))

(define java-exception? 
  (condition-predicate 'java))

(define java-exception-message
  (condition-property-accessor 'java 'message #f))

(define java-exception-trace
  (condition-property-accessor 'java 'trace #f))

(define java-exception-type
  (condition-property-accessor 'java 'type #f))

(define java-condition-handler
  (let ((o (current-exception-handler)))
    (lambda (exception)
      (if (java-exception? exception)
        (let ((trace   (java-exception-trace exception))
              (message (java-exception-message exception)))
          (newline)
          (print trace)
          (abort (make-property-condition 'exn 'message "java-exception-handler returned")))
        (o exception)))))

(current-exception-handler java-condition-handler)

(define (check-jexception v)
  (let ((e (exception-occurred)))
    (if e
      (abort (make-condition e))
      v)))
