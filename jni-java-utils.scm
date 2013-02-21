;; jni-java-utils.scm: high level java procedures

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
    (let* ((Object.toString/method (method java.lang.Object java.lang.String toString))
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
  (jlambda-method #f java.lang.Class boolean isPrimitive))
(define Class.getMethods
  (jlambda-method #f java.lang.Class #(java.lang.reflect.Method) getMethods))
(define Class.getDeclaredMethods
  (jlambda-method #f java.lang.Class #(java.lang.reflect.Method) getDeclaredMethods))

(define Method.getModifiers
  (jlambda-method #f java.lang.reflect.Method int getModifiers))
(define Method.getReturnType
  (jlambda-method #f java.lang.reflect.Method java.lang.Class getReturnType))
(define Method.getName
  (jlambda-method #f java.lang.reflect.Method java.lang.String getName))
(define Method.getParameterTypes
  (jlambda-method #f java.lang.reflect.Method #(java.lang.Class) getParameterTypes))
