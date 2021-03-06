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

(define class-cache (make-lru-cache 100 eq? (lambda (name jclass)
                                              (delete-global-ref jclass))))

(define find-class/jni
  (let ((f (jni-env-lambda jclass FindClass (const c-string))))
    (lambda (name)
      (let* ((s      (string->symbol name))
             (class  (lru-cache-ref class-cache s)))
        (if class
          class
          (let* ((class        (f name))
                 (global-class (if class (local->global class) class)))
            (if global-class
              (lru-cache-set! class-cache s global-class))
            global-class))))))

(define (join-class-pkg pkg class)
  (symbol-append (string->symbol pkg) '|.| class))

(define push-local-frame
  (let ((push (jni-env-lambda jint PushLocalFrame jint)))
    (lambda (size)
      (invoke-jni/safe (cut push size)))))

(define pop-local-frame
  (let ((pop (jni-env-lambda jobject PopLocalFrame jobject)))
    (lambda (jobject)
      (invoke-jni/safe (cut pop jobject)))))

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
  (let ((find-class/safe (lambda (c) (invoke-jni/safe (lambda () (find-class/jni c))))))
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

(define super-class 
  (let ((super-class/jni (jni-env-lambda jclass GetSuperclass jclass)))
    (lambda (jclass)
      (prepare-jobject (super-class/jni jclass)))))

(define get-object-class 
  (let ((get-object-class/jni (jni-env-lambda jclass GetObjectClass jobject)))
    (lambda (jobject)
      (prepare-jobject (get-object-class/jni jobject)))))

(define instance-of?
  (jni-env-lambda jboolean IsInstanceOf jobject jclass))
(define same-object?
  (jni-env-lambda jboolean IsSameObject jobject jobject))
(define assignable-from?
  (jni-env-lambda jboolean IsAssignableFrom jclass jclass))
(define new-object
  (jni-env-lambda jobject NewObjectA jclass jmethod-id jvalue))

(define get-field 
  (let ((get-field/jni (jni-env-lambda jfield-id GetFieldID jclass (const c-string) (const c-string))))
    (lambda (jclass name type)
      (invoke-jni/safe 
        (cut get-field/jni jclass name type)))))

(define get-static-field 
  (let ((get-static-field/jni (jni-env-lambda jfield-id GetStaticFieldID jclass (const c-string) (const c-string))))
    (lambda (jclass name type)
      (invoke-jni/safe 
        (cut get-static-field/jni jclass name type)))))

(define get-method-id 
  (let ((get-method-id/jni (jni-env-lambda jmethod-id GetMethodID jclass (const c-string) (const c-string))))
    (lambda (jclass name signature)
      (invoke-jni/safe (cut get-method-id/jni jclass name signature)))))

(define get-static-method-id 
  (let ((get-static-method-id/jni (jni-env-lambda jmethod-id GetStaticMethodID jclass (const c-string) (const c-string))))
    (lambda (jclass name signature)
      (invoke-jni/safe (cut get-static-method-id/jni jclass name signature)))))

(define make-jvalue-array
  (foreign-lambda jvalue make_jvalue_array int))
(define free-jvalue-array
  (foreign-lambda void free_jvalue_array jvalue))

(define make-array 
  (let ((make-array/jni (jni-env-lambda jobject-array NewObjectArray jsize jclass jobject)))
    (lambda (size jclass jobject)
      (prepare-jobject (make-array/jni size jclass jobject)))))

(define array-length
  (jni-env-lambda jsize GetArrayLength jarray))
(define array-ref
  (jni-env-lambda jobject GetObjectArrayElement jobject-array jsize))
(define array-set!
  (jni-env-lambda void SetObjectArrayElement jobject-array jsize jobject))

(define make-int-array
  (let ((make-int-array/jni (jni-env-lambda jobject NewIntArray jsize)))
    (lambda (size)
      (prepare-jobject (make-int-array/jni size)))))

(define get-int-array-elements
  (jni-env-lambda c-pointer GetIntArrayElements jobject c-pointer))

(define release-int-array-elements
  (jni-env-lambda void ReleaseIntArrayElements jobject c-pointer jint))

(define new-local-ref
  (jni-env-lambda jobject NewLocalRef jobject))
(define delete-local-ref
  (jni-env-lambda void DeleteLocalRef jobject))
(define new-global-ref
  (jni-env-lambda jobject NewGlobalRef jobject))
(define delete-global-ref
  (jni-env-lambda void DeleteGlobalRef jobject))

(define (delete-local-ref/if-jobject jobject)
  (if (jobject? jobject)
    (delete-local-ref jobject)))

(define-syntax let-local-refs*
  (syntax-rules ()
    ((_ () body ...) (begin body ...))
    ((_ ((name value) rest ...) body ...)
     (let* ((name value)
            (result (let-local-refs* (rest ...) body ...)))
       (delete-local-ref/if-jobject name)
       result))))

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

(define (jstring str)
  (and str
       (let ((len (string-length str))
             (out '()))
         (let loop ((pos 0))
           (if (= pos len)
               (let ((result (blob->string (u16vector->blob/shared (list->u16vector (reverse! out))))))
                 (prepare-jobject (jstring/jni result (quotient (string-length result) 2))))
               (let* ((head (char->integer (string-ref str pos)))
                      (rlen (cond ((= (bitwise-and head #x80) #x00) 1)
                                  ((= (bitwise-and head #xE0) #xC0) 2)
                                  ((= (bitwise-and head #xF0) #xE0) 3)
                                  ((= (bitwise-and head #xF8) #xF0) 4))))
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
                                          (let ((ptr (pointer+ ptr 2)))
                                            (if (pointer=? ptr end)
                                                (error "Incomplete UTF-16 surrogate pair" jstring (get-output-string out))
                                                (let ((trail (pointer-u16-ref ptr)))
                                                  (cons (utf16-pair->char lead trail) ptr))))))))
                      (display (##sys#char->utf8-string (car result)) out)
                      (loop (pointer+ (cdr result) 2)))))))))))

(define (array->list array-object)
  (do ((idx 0 (+ idx 1))
       (object-list '() (cons (array-ref array-object idx) object-list)))
    ((<= (array-length array-object) idx) object-list)))

(define (array-map! array-object f #!key (disposal delete-local-ref))
  (do ((idx 0 (+ idx 1)) (object-list '() 
                                      (let* ((e (array-ref array-object idx))
                                             (r (cons (f e) object-list)))
                                        (if (and disposal
                                                 (jobject? e))
                                          (disposal e))
                                        r)))
    ((<= (array-length array-object) idx) object-list)))

(define (list->array! class lst #!key (disposal delete-local-ref))
  (let ((len (length lst)))
    (let ((arr (make-array len class #f)))
      (let loop ((i 0) (lst lst))
        (if (null? lst)
          arr
          (begin
            (let ((e (car lst)))
              (array-set! arr i e)
              (if (and disposal
                       (jobject? e))
                (disposal e))
              (loop (+ i 1) (cdr lst)))))))))

(define (list->array/map class proc lst #!optional (dispose delete-local-ref))
  (let ((arr (make-array (length lst) class #f)))
    (let loop ((i 0) (lst lst))
      (if (null? lst)
          arr
          (let ((e (proc (car lst))))
            (array-set! arr i e)
            (when (and dispose (jobject? e))
              (dispose e))
            (loop (+ i 1) (cdr lst)))))))

;; TODO: Generate corresponding procedures for the other primitive array types too
(define (list->int-array lst)
  (let* ((jarr (make-int-array (length lst)))
         (arr (get-int-array-elements jarr #f)))
    (let loop ((i 0) (lst lst))
      (if (null? lst)
          (begin
            (release-int-array-elements jarr arr 0)
            jarr)
          (begin
            (pointer-s32-set! (pointer+ arr i) (car lst))
            (loop (+ i 4) (cdr lst)))))))

(define (int-array->list jarr)
  (let ((arr (get-int-array-elements jarr #f)))
    (let loop ((ptr (pointer+ arr (* 4 (array-length jarr))))
               (lst '()))
      (if (pointer=? ptr arr)
          (begin
            (release-int-array-elements jarr arr (foreign-value JNI_ABORT jint))
            lst)
          (let ((ptr (pointer+ ptr -4)))
            (loop ptr
                  (cons (pointer-s32-ref ptr) lst)))))))

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
    (prepare-jobject (new-object jclass jmethod jvalues))
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

