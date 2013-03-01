(use jni lolevel expand-full moremacros srfi-13)

(jvm-init)

(define jstring-value-of
  (jlambda-method (static) java.lang.String java.lang.String valueOf int))

(define jstring-contains
  (jlambda-method #f boolean java.lang.String contains java.lang.CharSequence))

(let ((eleven (jstring-value-of 11)))
  (print (jstring->string eleven)) ; => "11"
  (print (jstring-contains eleven (jstring-value-of 1)))) ; => #t

(define init-Integer-int (jlambda-constructor java.lang.Integer int))

(define b (init-Integer-int 1))

(define Integer-toString
  (jlambda-method #f java.lang.String java.lang.Integer toString))

(print (Integer-toString b))
(print (jstring->string (Integer-toString b)))
