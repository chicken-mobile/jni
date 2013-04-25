;; check test.jar or java/jni-test project to see the test classes used in this file

(use jni lolevel expand-full moremacros srfi-13 test)
(import-for-syntax test)

(jvm-init "tests/test.jar:java/misc-utils.jar")

(define-syntax test-jstring
  (syntax-rules ()
    ((_ str jstring)
     (test str (jstring->string jstring)))))

(define-syntax test-class
  (syntax-rules ()
    ((_ class expr)
     (test (string-append "class " (symbol->string 'class)) (to-string expr)))))
    
(define new-Foo (jlambda-constructor com.chicken_mobile.jni.test.Foo))
(define new-Bar (jlambda-constructor com.chicken_mobile.jni.test.Bar int))
(define new-N1 (jlambda-constructor com.chicken_mobile.jni.test.N1))
(define new-N2 (jlambda-constructor com.chicken_mobile.jni.test.N2))
(define new-Integer (jlambda-constructor java.lang.Integer int))

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

            (let ((foo (new-Foo))
                  (Foo-bar (jlambda-field (private final) com.chicken_mobile.jni.test.Bar com.chicken_mobile.jni.test.Foo bar))
                  (Bar-id (jlambda-field #f int com.chicken_mobile.jni.test.Bar id)))
              (let* ((bar (Foo-bar foo)))
                (test 11 (Bar-id bar))))
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

(test-group "jlambda-methods"

            (let* ((bar (new-Bar 1))
                   (ov1 (jlambda-methods 'com.chicken_mobile.jni.test.Bar 'ov1 
                                         '((#f int) 
                                           (#f int int) 
                                           (#f int java.lang.String) 
                                           (#f int com.chicken_mobile.jni.test.Bar)
                                           (#f int int java.lang.String)
                                           (#f int int com.chicken_mobile.jni.test.Bar)
                                           (#f int long)
                                           (#f int short)
                                           (#f int long int) 
                                           (#f int float)
                                           (#f int double)
                                           (#f int int long)
                                           (#f int int int)
                                           (#f int com.chicken_mobile.jni.test.N1)
                                           (#f int com.chicken_mobile.jni.test.N2)
                                           (#f int java.lang.Integer)
                                           (#f int char)))))
                (test 01 (ov1 bar))
                (test 02 (ov1 bar "hola"))
                (test 03 (ov1 bar 3))
                (test 04 (ov1 bar bar))
                (test 05 (ov1 bar 1 "hola"))
                (test 06 (ov1 bar 2 bar))
                (test 07 (ov1 bar (expt 2 32)))
                (test 08 (ov1 bar (type: short 1)))
                (test 09 (ov1 bar (type: long 1) 1))
                (test 10 (ov1 bar 1.3))
                (test 10 (ov1 bar (type: float 1)))
                (test 11 (ov1 bar ((jlambda-field (static) double java.lang.Double MIN_VALUE))))
                (test 12 (ov1 bar 1 (expt 2 32))) 
                (test 13 (ov1 bar 10 10))
                (test 14 (ov1 bar (new-N1)))
                (test 14 (ov1 bar (type: com.chicken_mobile.jni.test.N1 (new-N2))))
                (test 15 (ov1 bar (new-N2)))
                (test 16 (ov1 bar (new-Integer 1)))
                (test 17 (ov1 bar #\c)))

            (let* ((bar (new-Bar 1))
                   (ov1 (jlambda-methods 'com.chicken_mobile.jni.test.Bar 'ov1 
                                         '((#f int) 
                                           (#f int java.lang.String) 
                                           (#f int com.chicken_mobile.jni.test.Bar)
                                           (#f int int java.lang.String)
                                           (#f int int com.chicken_mobile.jni.test.Bar)
                                           (#f int long)
                                           (#f int short)
                                           (#f int float)
                                           (#f int double)
                                           (#f int int long)
                                           (#f int int int)
                                           (#f int com.chicken_mobile.jni.test.N1)
                                           (#f int com.chicken_mobile.jni.test.N2)
                                           (#f int java.lang.Integer)
                                           (#f int char)))))
                (test 7 (ov1 bar 3))) ; not defined for int so prefer long

            (let* ((bar (new-Bar 1))
                   (ov1 (jlambda-methods 'com.chicken_mobile.jni.test.Bar 'ov1 
                                         '((#f int) 
                                           (#f int java.lang.String) 
                                           (#f int com.chicken_mobile.jni.test.Bar)
                                           (#f int int java.lang.String)
                                           (#f int int com.chicken_mobile.jni.test.Bar)
                                           (#f int short)
                                           (#f int float)
                                           (#f int double)
                                           (#f int int long)
                                           (#f int int int)
                                           (#f int com.chicken_mobile.jni.test.N1)
                                           (#f int com.chicken_mobile.jni.test.N2)
                                           (#f int java.lang.Integer)
                                           (#f int char)))))
                (test 10 (ov1 bar 3))) ; not defined for int or long so prefer float


            (let* ((bar (new-Bar 1)) ; testing diff return-types
                   (ov2 (jlambda-methods 'com.chicken_mobile.jni.test.Bar 'ov2 
                                         '((#f int int) 
                                           (#f java.lang.String java.lang.String)))))
                (test 1 (ov2 bar 3)) 
                (test-jstring "ov2" (ov2 bar "hi")))
            ); end jlambda-methods tests

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
            (import-java-ns ((java.lang String)))

            (test-class java.lang.String (class java.lang.String))
            (test-class java.lang.String (class String))
            (test-error (class OtherString))

            (import-table #f) ; reset import-table

            (import-java-ns ((java.lang *)
                             (com.chicken_mobile.jni.test *)))

            (test-class java.lang.String (class java.lang.String))
            (test-class java.lang.String (class String))
            (test-class java.lang.System (class System))
            (test-class java.lang.Short (class Short))
            (test-error (class OtherString))

            (import-table #f) ; reset import-table

            (import-java-ns ((java.lang (String System))))

            (test-class java.lang.String (class java.lang.String))
            (test-class java.lang.String (class String))
            (test-class java.lang.System (class System))
            (test-error (class OtherString))

            (import-table #f) ; reset import-table

            (import-java-ns ((java.lang *)
                             (com.chicken_mobile.jni.test *)))

            (let ((jstring-value-of (jlambda-method (static) java.lang.String java.lang.String valueOf int))
                  (jstring-value-of2 (jlambda-method (static) String String valueOf int)))
              (test-jstring "11" (jstring-value-of 11))
              (test-jstring "11" (jstring-value-of2 11)))

            (let ((foo-noSense (jlambda-field (static) String Foo noSense))
                  (foo-sense (jlambda-field (static) String Foo sense)))
              (set! (foo-noSense) (foo-sense)))

            (import-table #f) ; reset import-table

            (import-java-ns ((java.lang *)
                             (com.chicken_mobile.jni.test (Bar Foo))))

            (let ((foo (new-Foo))
                  (Foo-bar (jlambda-field (private final) Bar Foo bar))
                  (Bar-id (jlambda-field #f int Bar id)))
              (let* ((bar (Foo-bar foo)))
                (test 11 (Bar-id bar))))
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

            (let* ((bar (new-Bar 1))
                   (ov1 (jlambda com.chicken_mobile.jni.test.Bar ov1)))
              (test 01 (ov1 bar))
              (test 02 (ov1 bar "hola"))
              (test 03 (ov1 bar 3))
              (test 04 (ov1 bar bar))
              (test 05 (ov1 bar 1 "hola"))
              (test 06 (ov1 bar 2 bar))
              (test 07 (ov1 bar (expt 2 32)))
              (test 10 (ov1 bar 1.3))
              (test 11 (ov1 bar ((jlambda-field (static) double java.lang.Double MIN_VALUE))))
              (test 12 (ov1 bar 1 (expt 2 32))) 
              (test 13 (ov1 bar 10 10))
              (test 14 (ov1 bar (new-N1)))
              (test 15 (ov1 bar (new-N2)))
              (test 16 (ov1 bar (new-Integer 1)))
              (test 17 (ov1 bar #\c)))

            (let ((new-Bar (jlambda com.chicken_mobile.jni.test.Bar new)))
              (test-class com.chicken_mobile.jni.test.Bar (get-object-class (new-Bar 1)))
              (test-class com.chicken_mobile.jni.test.Bar (get-object-class (new-Bar)))
              (test-class com.chicken_mobile.jni.test.Bar (get-object-class (new-Bar " "))))

            ); end jlambda test group

(test-group "jimport"

            (jimport java.lang.String)
            (test "class java.lang.String$CaseInsensitiveComparator" (to-string (get-object-class (CASE_INSENSITIVE_ORDER))))
            (test-jstring "1" (valueOf 1))

            (jimport com.chicken_mobile.jni.test.Bar (prefix <> bar-))
            (test 1 (bar-ov1 (bar-new)))

            (jimport com.chicken_mobile.jni.test.Handler (prefix <> handler-))
            (test 1 (handler-send (bar-handler (bar-new)) 0))

            (jimport java.lang.String (prefix (only <> valueOf) String-))
            (test-jstring "1" (String-valueOf 1))

            ); end jimport test group

(test-exit)
