(use jni lolevel expand-full moremacros srfi-13 test)

;; Foo.java
;; 
;;  package com.bevuta.testapp;
;;  
;;  public class Foo {
;;  	
;;  	public static boolean lie = true;
;;  	public static String noSense = "lil oiuy pppq";
;;   	public static String sense = "cogito ergo sum";
;;  	public int number;
;;  	private int secret_number;
;;  
;;  	public Foo() {
;;  		number = 12;
;;  		secret_number = 8;  //¡¡¡¡¡¡¡¡ oohhh  !!!!!!
;;  	}
;;  	
;;  }

(jvm-init "tests/test.jar")

(define (test-jstring str jstring)
	(test str (jstring->string jstring)))

(test-group "jlambda-field"

						(define new-Foo (jlambda-constructor com.bevuta.testapp.Foo))

						(let ((test-lie (jlambda-field (static) boolean com.bevuta.testapp.Foo lie)))
							(test #t (test-lie))
							(set! (test-lie) #f)
							(test #f (test-lie)))

						(let ((test-noSense (jlambda-field (static) java.lang.String com.bevuta.testapp.Foo noSense))
									(test-sense (jlambda-field (static) java.lang.String com.bevuta.testapp.Foo sense)))
							(test-jstring "lil oiuy pppq" (test-noSense))
							(test-jstring "cogito ergo sum" (test-sense))
							(set! (test-noSense) (test-sense))
							(test-jstring "cogito ergo sum" (test-noSense)))

						(let ((test-number (jlambda-field () int com.bevuta.testapp.Foo number))
									(o (new-Foo)))
							(test 12 (test-number o))
							(set! (test-number o) 300)
							(test 300 (test-number o)))

						(let ((o (new-Foo)))
							(let ((test-secret_number (jlambda-field (private) int com.bevuta.testapp.Foo secret_number)))
								(test 8 (test-secret_number o))
								(set! (test-secret_number o) 33)
								(test 33 (test-secret_number o)))

							;!!!!! Notice that the modifier can be omitted: (all but static)
							(let ((test-secret_number (jlambda-field () int com.bevuta.testapp.Foo secret_number))) 
								(test 33 (test-secret_number o))
								(set! (test-secret_number o) 55)
								(test 55 (test-secret_number o))))
						); end jlambda-field test group

(test-group "jlambda-method"
						(define jstring-value-of
							(jlambda-method (static) java.lang.String java.lang.String valueOf int))

						(define jstring-contains
							(jlambda-method #f boolean java.lang.String contains java.lang.CharSequence))

						(let ((eleven (jstring-value-of 11)))
							(test-jstring "11" eleven)
							(test #t (jstring-contains eleven (jstring-value-of 1))))

            (begin
              (test-error ((jlambda-method (static) boolean java.lang.String contains2)))
              (exception-clear))

						); end jlambda-method test group

(test-group "jlambda-constructor"
						(define new-Integer-int (jlambda-constructor java.lang.Integer int))

						(define Integer-toString
							(jlambda-method #f java.lang.String java.lang.Integer toString))

						(let ((n (new-Integer-int 30)))
							(test-jstring "30" (Integer-toString n)))
						); end jlambda-constructor test group

(test-exit)
