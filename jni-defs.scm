;; generate type variant procedures
(include "jni-def-macros.scm")
(define-call-procs Void void)
(define-type-procs)
(define-get-field-procs)
(define-jni-modifier-procs)
;;

(use ports extras srfi-4)

(define (invoke-jni/safe thunk)
  (let* ((r (thunk)))
    (if (exception-check) 
      (exception-clear))
    r))

(define version
  (jni-env-lambda jint GetVersion))

(define find-class/jni
  (jni-env-lambda jclass FindClass (const c-string)))

(define (join-class-pkg pkg class)
  (symbol-append (string->symbol pkg) '|.| class))

(define (make-import-table imports)
  (let loop ((imports imports)
             (classes '())
             (pkgs    '()))
    (if (null? imports)
      (append classes (list (cons '* pkgs)))
      (let* ((import   (car imports))
             (pkg      (symbol->string (car import)))
             (s-class  (cadr import)))
        (if (eq? s-class '*)
          (loop (cdr imports) classes (cons pkg pkgs))
          (loop (cdr imports) 
                (if (list? s-class)
                  (append (map (lambda (x) (cons x (join-class-pkg pkg x))) s-class) classes)
                  (cons (cons s-class (join-class-pkg pkg s-class)) classes))
                pkgs))))))

(define (find-class/by-pkgs find-class class pkgs)
  (call/cc (lambda (found)
             (fold (lambda (import _)
                     (let* ((class-name   (join-class-pkg import class))
                            (mangled-name (mangle-class-name class-name))
                            (r            (find-class mangled-name)))
                       (if r (found r) #f)))
                   '() pkgs))))

(define import-table (make-parameter #f))

(define (find-class class)
  (let ((find-class/safe (lambda (c) (invoke-jni/safe (lambda () (prepare-local-jobject (find-class/jni c)))))))
    (if (import-table)
      (let ((s-class (string->symbol class)))
        (or (find-class/safe class)
            (let ((import (assq s-class (import-table))))
              (and import 
                   (find-class/safe (mangle-class-name (cdr import)))))
            (let ((pkgs (cdr (assq '* (import-table)))))
              (and (not (null? pkgs))
                   (find-class/by-pkgs find-class/safe s-class pkgs)))))
      (find-class/safe class))))

(define (find-class/or-error name)
  (let ((class (find-class (mangle-class-name name))))
    (if class
      class
      (error 'find-class/or-error "class not found" name))))

(define super-class/jni
  (jni-env-lambda jclass GetSuperclass jclass))
(define (super-class jclass)
  (prepare-local-jobject (super-class/jni jclass)))

(define get-object-class/jni
  (jni-env-lambda jclass GetObjectClass jobject))
(define (get-object-class jobject)
  (prepare-local-jobject (get-object-class/jni jobject)))

(define instance-of?
  (jni-env-lambda jboolean IsInstanceOf jobject jclass))
(define same-object?
  (jni-env-lambda jboolean IsSameObject jobject jobject))
(define assignable-from?
  (jni-env-lambda jboolean IsAssignableFrom jclass jclass))
(define new-object
  (jni-env-lambda jobject NewObjectA jclass jmethod-id jvalue))

(define get-field/jni
  (jni-env-lambda jfield-id GetFieldID jclass (const c-string) (const c-string)))
(define get-static-field/jni
  (jni-env-lambda jfield-id GetStaticFieldID jclass (const c-string) (const c-string)))
(define get-method-id/jni
  (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string)))
(define get-static-method-id/jni
  (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string)))

(define (get-field jclass name type)
  (invoke-jni/safe (lambda () (get-field/jni jclass name type))))
(define (get-static-field jclass name type)
  (invoke-jni/safe (lambda () (get-static-field/jni jclass name type))))
(define (get-method-id jclass name signature)
  (invoke-jni/safe (lambda () (get-method-id/jni jclass name signature))))
(define (get-static-method-id jclass name signature)
  (invoke-jni/safe (lambda () (get-static-method-id/jni jclass name signature))))

(define make-jvalue-array
  (foreign-lambda jvalue make_jvalue_array int))
(define free-jvalue-array
  (foreign-lambda void free_jvalue_array jvalue))

(define make-array/jni
  (jni-env-lambda jobject-array NewObjectArray jsize jclass jobject))
(define (make-array size jclass jobject)
  (prepare-local-jobject (make-array/jni size jclass jobject)))

(define array-length
  (jni-env-lambda jsize GetArrayLength jarray))
(define array-ref
  (jni-env-lambda jobject GetObjectArrayElement jobject-array jsize))
(define array-set!
  (jni-env-lambda void SetObjectArrayElement jobject-array jsize jobject))

(define new-local-ref
  (jni-env-lambda jobject NewLocalRef jobject))
(define delete-local-ref
  (jni-env-lambda void DeleteLocalRef jobject))
(define new-global-ref
  (jni-env-lambda jobject NewGlobalRef jobject))
(define delete-global-ref
  (jni-env-lambda void DeleteGlobalRef jobject))

(define exception-check
  (jni-env-lambda jboolean ExceptionCheck))
(define exception-clear
  (jni-env-lambda void ExceptionClear))
(define exception-describe
  (jni-env-lambda void ExceptionDescribe))
(define exception-occurred
  (jni-env-lambda jthrowable ExceptionOccurred))

(define field-id->Field
  (jni-env-lambda jobject ToReflectedField jclass jfield-id jboolean))
(define Field->field-id
  (jni-env-lambda jobject FromReflectedField jobject))
(define method-id->Method
  (jni-env-lambda jobject ToReflectedMethod jclass jmethod-id jboolean))
(define Method->method-id
  (jni-env-lambda jobject FromReflectedMethod jobject))

(define monitor-enter
  (jni-env-lambda jint MonitorEnter jobject))
(define monitor-exit
  (jni-env-lambda jint MonitorExit jobject))

(define (expand-type type #!optional return)
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
         (and-let* ((return (expand-type return)))
           (string-append "(" (string-intersperse (map expand-type type) "") ")" return)))
        (else 
          #f)))

(define-syntax type-signature
  (er-macro-transformer
    (lambda (x r c)
      (let ((%expand-type   (r 'expand-type))
            (type           (cadr x))
            (return         (and (pair? (cddr x)) (caddr x))))
        `(or (,%expand-type ,type ,return)
             (error "Invalid Java type signature" ,type ,return))))))



;; As per http://www.unicode.org/faq/utf_bom.html#utf16-4
(define +utf16-lead-offset+ (- #xD800 (arithmetic-shift #x10000 -10)))
(define +utf16-surrogate-offset+ (- #x10000 (arithmetic-shift #xD800 10) #xDC00))

(define (codepoint->utf16-lead cp)
  (+ +utf16-lead-offset+ (arithmetic-shift cp -10)))

(define (codepoint->utf16-trail cp)
  (+ #xDC00 (bitwise-and cp #x3FF)))

(define (utf16-pair->char lead trail)
  (integer->char (+ (arithmetic-shift lead 10) trail +utf16-surrogate-offset+)))

(define jstring/jni
  (jni-env-lambda jstring NewString scheme-pointer jsize))

(define (write-short s out)
  (write-string (blob->string (u16vector->blob/shared (u16vector s))) 2 out))

(define (jstring str)
  (and str
       (let ((len (string-length str))
             (out '()))
         (let loop ((pos 0))
           (if (= pos len)
               (let ((result (blob->string (u16vector->blob/shared (list->u16vector (reverse! out))))))
                 (prepare-local-jobject (jstring/jni result (quotient (string-length result) 2))))
               (let* ((head (char->integer (string-ref str pos)))
                      (rlen (case (bitwise-and #xF0 head)
                              ((#xF0) 4)
                              ((#xE0) 3)
                              ((#xC0) 2)
                              (else   1))))
                 (if (= 1 rlen)
                     (set! out (cons head out))
                     (let ((offset (* 6 (- rlen 1))))
                       (let loop2 ((codepoint (arithmetic-shift head offset))
                                   (pos (+ pos 1))
                                   (offset offset))
                         (if (zero? offset)
                             (let ((codepoint (bitwise-and codepoint
                                                           (case rlen
                                                             ((2) #x7FF)
                                                             ((3) #xFFFF)
                                                             ((4) #x1FFFFF)))))
                               (set! out
                                     (if (> codepoint #xFFFF)
                                         (cons (codepoint->utf16-trail codepoint)
                                               (cons (codepoint->utf16-lead codepoint) out))
                                         (cons codepoint out))))
                             (let* ((offset (- offset 6))
                                    (cont (char->integer (string-ref str pos)))
                                    (cont (arithmetic-shift cont offset))
                                    (cont (bitwise-and cont
                                                       (case offset
                                                         ((0) #x3F)
                                                         ((6) #xFFF)
                                                         ((12) #x3FFFF)))))
                               (loop2 (bitwise-ior codepoint cont)
                                      (+ pos 1)
                                      offset))))))
                 (loop (+ pos rlen))))))))

(define jstring->string
  (let ((get-chars     (jni-env-lambda (c-pointer (const jchar)) GetStringChars jstring c-pointer))
        (release-chars (jni-env-lambda void ReleaseStringChars jstring (c-pointer (const jchar))))
        (get-length    (jni-env-lambda jsize GetStringLength jstring)))
    (lambda (jstring)
      (let* ((chars (get-chars jstring #f)) 
             (len   (get-length jstring))
             (end   (pointer+ chars (* 2 len))))
        (call-with-output-string
            (lambda (out)
              (let loop ((ptr chars))
                (if (pointer=? ptr end)
                    (release-chars jstring chars)
                    (let* ((lead (pointer-u16-ref ptr))
                           (result (cond ((< lead 0)
                                          (error "Invalid UTF-16" jstring))
                                         ((or (< 0 lead #xD7FF)
                                              (< #xE000 lead #xFFFF))
                                          (cons (integer->char lead) ptr))
                                         (else
                                          (let* ((ptr (pointer+ ptr 2))
                                                 (trail (pointer-u16-ref ptr)))
                                            (cons (utf16-pair->char lead trail) ptr))))))
                      (display (##sys#char->utf8-string (car result)) out)
                      (loop (pointer+ (cdr result) 2)))))))))))

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

(define-syntax list->array/map 
  (ir-macro-transformer
    (lambda (x i c)
      (let* ((class-name (cadr x))
             (mapper     (caddr x))
             (list       (cadddr x)))
        `(list->array (class ,class-name)
                      (map ,mapper ,list))))))

(define (get-type-symbol type-name)
  (if (string-prefix? "L" type-name)
    (string->symbol (string-drop (string-drop-right type-name 1) 1)) ; Lclass;
    (cond ((string=? type-name "Z") 'boolean)
          ((string=? type-name "B") 'byte)
          ((string=? type-name "C") 'char)    
          ((string=? type-name "S") 'short)   
          ((string=? type-name "I") 'int)     
          ((string=? type-name "J") 'long)    
          ((string=? type-name "F") 'float)   
          ((string=? type-name "D") 'double)  
          ((string=? type-name "V") 'void)
          (#t (error 'get-type-symbol "wrong type" type-name)))))

(define (class->type c)
  (let* ((class-str (to-string c)))
    (cond ((string-prefix? "class [" class-str) ; ie: "class [Ljava.lang.reflect.Method;"
           (vector (get-type-symbol (string-drop class-str (string-length "class [")))))
          ((string-prefix? "class " class-str)
           (string->symbol (string-drop class-str (string-length "class "))))
          ((string-prefix? "interface " class-str)
           (string->symbol (string-drop class-str (string-length "interface "))))
          (#t
           (string->symbol class-str)))))

(define (catch thunk on-error)
  (call/cc (lambda (k)
             (with-exception-handler (lambda (e) (k on-error))
                                     thunk))))

(define to-string
  (lambda (object)
    (let* ((Object.toString/method (get-method-id (find-class/or-error 'java.lang.Object) "toString" "()Ljava/lang/String;"))
           (String/instance (call-object-method object Object.toString/method #f))
           (string (jstring->string String/instance)))
      (delete-local-ref String/instance) string)))

(define (call-new jclass jmethod jvalues)
  (if jmethod 
    (prepare-local-jobject (new-object jclass jmethod jvalues))
    (error 'call-new "method not found")))

(define-syntax define-method
  (ir-macro-transformer
    (lambda (x i c)
      (let* ((name (mangle-method-name (strip-syntax (caadr x)))))
        `(define-external (,(i name)
                            (,(i '(c-pointer "JNIEnv")) env)
                            (,(i '(c-pointer "jobject")) ,(cadadr x))
                            . ,(cddadr x))
                          ,(i (caddr x))
                          (parameterize ((jni-env env)) . ,(cdddr x)))))))

