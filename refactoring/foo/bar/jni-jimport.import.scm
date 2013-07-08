;;;; jni-jimport.import.scm - GENERATED BY CHICKEN 4.8.2 -*- Scheme -*-

(eval '(import chicken scheme))
(import
  chicken
  extras
  matchable
  jni-lolevel
  jni-types
  jni-array
  jni-reflection
  jni-jlambda-methods)
(##core#begin
  (require-library
    matchable
    chicken
    extras
    jni-lolevel
    jni-types
    jni-array
    jni-reflection
    jni-jlambda-methods)
  (attach-thread))
(##core#begin
  (define (%array->alist getter predicate predicate-getter array)
    (array-fold
      (lambda (element alist)
        (let* ((key (getter element)) (value (alist-ref key alist eq? '())))
          (if (predicate (predicate-getter element))
            (alist-update! key (cons element value) alist)
            alist)))
      '()
      array)))
(##core#begin
  (define %method-name-alist
    (cute %array->alist method-name public? method-modifiers <>)))
(##core#begin
  (define (Class->signature type)
    (if (class-array? type)
      `#(,(class-name (class-component-type type)))
      (class-name type))))
(##core#begin
  (define (make-method-signatures methods)
    (fold (lambda (method result)
            (let ((modifier
                    (or (and (static? (method-modifiers method)) 'static)
                        'nonstatic))
                  (parameter-types
                    (array-map
                      Class->signature
                      (method-parameter-types method))))
              (alist-update!
                modifier
                (cons method (alist-ref modifier result eq? '()))
                result)))
          '()
          methods)))
(##core#begin
  (define (Method->jlambda-method method)
    (let ((modifer
            (or (and (static? (method-modifiers method)) 'static) 'nonstatic))
          (return-type (Class->signature (method-return-type method)))
          (identifier (method-name method))
          (parameters
            (array-map Class->signature (method-parameter-types method))))
      `(,modifer ,return-type ,identifier ,@parameters))))
(##core#begin
  (define (%foof method-alist)
    (map (lambda (methods)
           (match (car methods)
                  ((modifier return-type method-name arg-types ...)
                   (if (> (length methods) 1)
                     (let ((arg-types
                             (map (lambda (x)
                                    (match x
                                           ((modifier return-type method-name arg-types ...)
                                            `(,modifier ,arg-types))))
                                  methods)))
                       `(,method-name (,return-type ,method-name ,arg-types)))
                     `(,method-name
                       (,modifier ,return-type ,method-name ,@arg-types))))))
         (filter-unsupported-methods method-alist))))
(##core#begin
  (define (filter-unsupported-methods method-alist)
    (map (lambda (methods)
           (let ((return-type (method-return-type (cadr methods))))
             (fold (lambda (method methods)
                     (if (same-object? (method-return-type method) return-type)
                       (cons (Method->jlambda-method method) methods)
                       (begin
                         (print "warining: overloaded methods with differing return-types are currently not supported!")
                         (print (format
                                  "warining: this method will be ignored: ~A"
                                  method))
                         methods)))
                   '()
                   (cdr methods))))
         method-alist)))
(##core#begin
  (define (%jlambda-method-signatures class-object)
    (%foof (%method-name-alist (class-declared-methods class-object)))))
(##core#begin
  (define (%exported-method-names class-object)
    (fold (lambda (method exported-methods)
            (if (find (cut eq? method <>) exported-methods)
              exported-methods
              (cons method exported-methods)))
          '()
          (array-map method-name (class-methods class-object)))))
(##core#begin
  (define (%import-superclasses class-object)
    (let* ((superclass (class-superclass class-object)))
      (if superclass
        (let ((declared-method-names
                (map car
                     (%method-name-alist
                       (class-declared-methods class-object))))
              (superclass-name (class-name superclass)))
          `((import (except ,superclass-name ,@declared-method-names))))
        '()))))
(##core#begin
  (define (jimport-dependencies class-object)
    (drop-right!
      (fold append
            '()
            (map (lambda (class-object)
                   (reverse
                     (cons class-object
                           (array-fold
                             cons
                             '()
                             (class-interfaces class-object)))))
                 (let loop ((class-object class-object)
                            (superclass (class-superclass class-object)))
                   (if superclass
                     (cons class-object
                           (loop superclass (class-superclass superclass)))
                     (list class-object)))))
      1)))
(##sys#register-compiled-module
  'jni-jimport
  (list)
  '()
  (list (cons '%jimport
              (er-macro-transformer
                (lambda (x i c)
                  (match (strip-syntax x)
                         ((_ name)
                          (let ((class-object (class* name)))
                            (cons 'module
                                  `(,name
                                    ,(%exported-method-names class-object)
                                    (import
                                      (prefix (only chicken define use) %%)
                                      (only data-structures ->string)
                                      (prefix (only scheme define begin) %%)
                                      (only scheme quote)
                                      (prefix (only chicken print) %%)
                                      (prefix (only jni-types class) %%)
                                      jni-jlambda-methods)
                                    (%%use jni-jlambda-methods)
                                    ,@(%import-superclasses class-object)
                                    (%%define class-object (%%class ,name))
                                    (define-jlambda-methods
                                      class-object
                                      ,@(%jlambda-method-signatures class-object))))))))))
        (cons 'jimport
              (er-macro-transformer
                (lambda (x i c)
                  (let ((import
                          (let loop ((import (cadr (strip-syntax x))))
                            (or (and (list? import) (loop (cadr import)))
                                import))))
                    (let* ((class-object (class* import))
                           (dependencies (jimport-dependencies class-object)))
                      `(begin
                         ,@(map (lambda (dependency)
                                  `(%jimport ,(class-name dependency)))
                                (reverse
                                  (cons class-object (reverse dependencies))))
                         (import ,(cadr (strip-syntax x))))))))))
  (list))

;; END OF FILE
