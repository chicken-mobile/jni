(use jni lolevel expand-full moremacros srfi-13)

;; Test.java
;; 
;; package com.bevuta.testapp;
;; 
;; public class Test {
;; 	
;; 	public static boolean mentira = true;
;; 	public static String noSense = "lil oiuy pppq";
;;  public static String sense = "cogito ergo sum";
;; 	public int number;
;; 	private int secret_number;
;; 
;; 	
;; 	public Test() {
;; 		number = 12;
;; 		secret_number = 8;  //¡¡¡¡¡¡¡¡ oohhh  !!!!!!
;; 	}
;; 	
;; }

(jvm-init "examples/atest.jar")

(define-syntax new
  (syntax-rules ()
    ((_ class-name)
     (new-object (class class-name) (constructor class-name)))))

(let ((test-mentira (jlambda-field (static) boolean com.bevuta.testapp.Test mentira)))
  (print (test-mentira)) ; => #t
  (set! (test-mentira) #f)
  (print (test-mentira))) ; => #f

(let ((test-noSense (jlambda-field (static) java.lang.String com.bevuta.testapp.Test noSense))
      (test-sense (jlambda-field (static) java.lang.String com.bevuta.testapp.Test sense)))
  (print (jstring->string (test-noSense))) ; => lil oiuy pppq
  (print (jstring->string (test-sense))) ; => cogito ergo sum
  (set! (test-noSense) (test-sense))
  (print (jstring->string (test-noSense)))) ; => cogito ergo sum

(let ((test-number (jlambda-field () int com.bevuta.testapp.Test number)))
  (let ((o (new com.bevuta.testapp.Test)))
    (print (test-number o))  ; => 12
    (set! (test-number o) 300)
    (print (test-number o))))  ; => 300

(let ((o (new com.bevuta.testapp.Test)))
  (let ((test-secret_number (jlambda-field (private) int com.bevuta.testapp.Test secret_number)))
    (print (test-secret_number o))  ; => 8
    (set! (test-secret_number o) 33)
    (print (test-secret_number o)))  ; => 33

  ;!!!!! Notice that the modifier can be omitted: (all but static)
  (let ((test-secret_number (jlambda-field () int com.bevuta.testapp.Test secret_number))) 
    (print (test-secret_number o))  ; => 33
    (set! (test-secret_number o) 55)
    (print (test-secret_number o))))  ; => 55
