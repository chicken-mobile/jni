(use jni-lolevel)
(import-for-syntax jni-lolevel)
(include "signature.scm")

(define (find-class/error name)
  (let ((class-object (find-class name)))
    (if (or class-object (and (exception-check) (not (exception-clear))))
	(prepare-local-jobject class-object)
	(error 'find-class (format "no class named \"~A\" found :(" name)))))

(define (class* class-name)
  (find-class/error (or (and (symbol? class-name) (mangle-class-name class-name)) class-name)))

(define-for-syntax (%find-class name safe?)
  (let* ((name (strip-syntax name))
	 (class-name (mangle-class-name name)))
    `(,(or (and safe? 'find-class) 'find-class/error) ,class-name)))

(define-syntax %class
  (ir-macro-transformer
   (lambda (x i c) (%find-class (cadr x) #t))))
(define-syntax class
  (ir-macro-transformer
   (lambda (x i c) (%find-class (cadr x) #f))))



