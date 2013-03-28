(use jni-jvm lolevel expand-full moremacros srfi-13 test)
(import-for-syntax test)

;; Foo.java
;; 
;;  package com.chicken_mobile.jni.test;
;;  
;;  public class Foo {
;;    
;;    public static boolean lie = true;
;;    public static String noSense = "lil oiuy pppq";
;;    public static String sense = "cogito ergo sum";
;;    public int number;
;;    private int secret_number;
;;  
;;    public Foo() {
;;      number = 12;
;;      secret_number = 8;  //¡¡¡¡¡¡¡¡ oohhh  !!!!!!
;;    }
;;      
;;    public void xxx() {
;;      throw new ProtocolException("bad protocol");
;;    }
;;  
;;    public String xxx2() {
;;      throw new RuntimeException("bad protocol");
;;    }
;;  }

(jvm-init "tests/test.jar:java/misc-utils.jar")

(begin-for-syntax
  (import chicken)
  (unless (jni-env)
    (jvm-init "tests/test.jar:java/misc-utils.jar")))

(define-syntax test-jstring
  (syntax-rules ()
    ((_ str jstring)
     (test str (jstring->string jstring)))))

(define-syntax test-class
  (syntax-rules ()
    ((_ class expr)
     (test (string-append "class " (symbol->string 'class)) (to-string expr)))))
    
(define new-Foo (jlambda-constructor com.chicken_mobile.jni.test.Foo))

(test-group "primitives"
            (test-class com.chicken_mobile.jni.test.Foo (get-object-class (new-Foo))))

(test-group "class"
            (test-class java.lang.System (class java.lang.System))
            (test-error (class System)))

(test-group "jlambda-field"

            (let ((foo-lie (jlambda-field (static) boolean com.chicken_mobile.jni.test.Foo lie)))
              (test #t (foo-lie))
              (set! (foo-lie) #f)
              (test #f (foo-lie)))

            (let ((foo-noSense (jlambda-field (static) java.lang.String com.chicken_mobile.jni.test.Foo noSense))
                  (foo-sense (jlambda-field (static) java.lang.String com.chicken_mobile.jni.test.Foo sense)))
              (test-jstring "lil oiuy pppq" (foo-noSense))
              (test-jstring "cogito ergo sum" (foo-sense))
              (set! (foo-noSense) (foo-sense))
              (test-jstring "cogito ergo sum" (foo-noSense)))

            (let ((foo-number (jlambda-field () int com.chicken_mobile.jni.test.Foo number))
                  (o (new-Foo)))
              (test 12 (foo-number o))
              (set! (foo-number o) 300)
              (test 300 (foo-number o)))

            (let ((o (new-Foo)))
              (let ((foo-secret_number (jlambda-field (private) int com.chicken_mobile.jni.test.Foo secret_number)))
                (test 8 (foo-secret_number o))
                (set! (foo-secret_number o) 33)
                (test 33 (foo-secret_number o)))

              ;!!!!! Notice that the modifier can be omitted: (all but static)
              (let ((foo-secret_number (jlambda-field () int com.chicken_mobile.jni.test.Foo secret_number))) 
                (test 33 (foo-secret_number o))
                (set! (foo-secret_number o) 55)
                (test 55 (foo-secret_number o))))
            (test-error "class not found" (jlambda-field () int com.chicken_mobile.jni.test.AFoo secret_number))
            (test #f (exception-check))
            (test-error "field not found" (jlambda-field () int java.lang.String size))
            (test #f (exception-check))

            ); end jlambda-field test group

(test-group "jlambda-method"
            ;test #(..) signature, if not defined raises an error
            (jlambda-method #f #(java.lang.reflect.Method) java.lang.Class getMethods)

            (define jstring-value-of
              (jlambda-method (static) java.lang.String java.lang.String valueOf int))

            (define jstring-contains
              (jlambda-method #f boolean java.lang.String contains java.lang.CharSequence))

            (define jstring-contains2 ;testing modifiers
              (jlambda-method (public)  boolean java.lang.String contains java.lang.CharSequence))

            (let ((eleven (jstring-value-of 11)))
              (test-jstring "11" eleven)
              (test #t (jstring-contains eleven (jstring-value-of 1)))
              (test #t (jstring-contains2 eleven (jstring-value-of 1))))

            (test-error "class not found" (jlambda-method (static) boolean AFoo hi))
            (test #f (exception-check))
            (test-error "method not found" (jlambda-method (static) boolean java.lang.String hi))
            (test #f (exception-check))
            ); end jlambda-method test group

(test-group "jlambda-constructor"
            (define new-Integer-int (jlambda-constructor java.lang.Integer int))

            (define Integer-toString
              (jlambda-method #f java.lang.String java.lang.Integer toString))

            (let ((n (new-Integer-int 30)))
              (test-jstring "30" (Integer-toString n)))

            (test-error "class not found" (jlambda-constructor java.lang.AInteger int))
            (test #f (exception-check))
            (test-error "method not found" (jlambda-constructor java.lang.Integer int int))
            (test #f (exception-check))

            ); end jlambda-constructor test group

(test-group "import-java-ns"
            (import-java-ns ((java.lang String))
                            (test-class java.lang.String (class java.lang.String))
                            (test-class java.lang.String (class String))
                            (test-error (class OtherString)))

            (import-java-ns ((java.lang *)
                             (com.chicken_mobile.jni.test *))
                            (test-class java.lang.String (class java.lang.String))
                            (test-class java.lang.String (class String))
                            (test-class java.lang.System (class System))
                            (test-class java.lang.Short (class Short))
                            (test-error (class OtherString)))

            (import-java-ns ((java.lang (String System)))
                            (test-class java.lang.String (class java.lang.String))
                            (test-class java.lang.String (class String))
                            (test-class java.lang.System (class System))
                            (test-error (class Short)))

            (import-java-ns ((java.lang *)
                             (com.chicken_mobile.jni.test *))

                            (let ((jstring-value-of 
                                    (jlambda-method (static) java.lang.String java.lang.String valueOf int)))
                              (test-jstring "11" (jstring-value-of 11)))

                            (let ((jstring-value-of 
                                    (jlambda-method (static) String String valueOf int)))
                              (test-jstring "11" (jstring-value-of 11))))

            ); end import-java-ns test group

(test-group "exceptions"

            (let ((foo-xxx2 (jlambda-method #f java.lang.String com.chicken_mobile.jni.test.Foo xxx2))
                  (o (new-Foo)))
              (test-error (foo-xxx2 o)))

            (define (exception-thunk)
              (let ((foo-xxx (jlambda-method #f void com.chicken_mobile.jni.test.Foo xxx))
                    (o (new-Foo)))
                (foo-xxx o)))

            (call/cc
              (lambda (k)
                (with-exception-handler (lambda (exception) 
                                          (test #t (java-exception? exception))
                                          (test "bad protocol" (java-exception-message exception))
                                          (test 'java.lang.RuntimeException (java-exception-type exception))
                                          (test #f (exception-check))
                                          (k '()))
                                        exception-thunk)))

            (test "exception match" #t
                  (condition-case (exception-thunk)
                    ((java java.lang.RuntimeException) #t)
                    (var () #f)))

            (test "exception match" #t
                  (condition-case (exception-thunk)
                    ((java.lang.RuntimeException) #t)
                    (var () #f)))

            (test "exception match" #t
                  (condition-case (exception-thunk)
                    ((java) #t)
                    (var () #f)))

            (test "exception match" #t
                  (condition-case (exception-thunk)
                    ((exn)  #t)
                    (var () #f)))
            ); end exceptions test group

(test-group "jlambda"
            ;jlambda class
            (test-class java.lang.System (jlambda java.lang.System))

            ;; jlambda field
            (let ((foo-number (jlambda com.chicken_mobile.jni.test.Foo number))
                  (o (new-Foo)))
              (test 12 (foo-number o))
              (set! (foo-number o) 300)
              (test 300 (foo-number o)))

            ;;;invalid 
            (begin-for-syntax
              (require-library test)
              (test-error (jlambda com.chicken_mobile.jni.test.Foo sense2)))

            ;;TODO: temp until jlambda-methods is ready
            ;(test '((java.nio.charset.Charset) (java.lang.String) (int #(byte) int int) ())
                  ;(jlambda java.lang.String getBytes))
            ); end jlambda test group

(test-exit)
