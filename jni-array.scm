(module jni-array
(array-fold array-fold* array-map array-map* array-find array-filter array->list list->array)
(import chicken scheme)
(use jni-lolevel)
(attach-thread)

(define (array-fold procedure knil array)
  (let ((l (array-length array)))
    (let loop ((knil knil) (idx 0))
      (if (= idx l)
	  knil (loop (procedure (array-ref array idx) knil) (+ 1 idx))))))

(define (array-fold* procedure knil array)
  (let ((l (array-length array)))
    (let loop ((knil knil) (idx 0))
      (if (= idx l)
	  knil (loop (procedure (array-ref array idx) idx knil) (+ 1 idx))))))

(define (array-map procedure array)
  (reverse
   (array-fold 
    (lambda (element result)
      (cons (procedure element) result)) '() array)))

(define (array-map* procedure array)
  (let ((array-length (array-length array))
	(mapped-array (make-array array-length)))
    (let loop ((idx 0))
      (or (and (= idx array-length) mapped-array)
	  (let ((array-element (array-ref array)))
	    (array-set! mapped-array idx (procedure array-element)) mapped-array)))))



(define (array-find predicate array)
  (call/cc 
   (lambda (return)
     (array-fold
      (lambda (element result)
	(if (predicate element) 
	    (return element)
	    #;(delete-local-ref element)
	    )) '() array))))

(define (array-filter predicate array)
  (array-fold
   (lambda (element result)
     (if (predicate element) 
	 (cons element result)
	 (begin (delete-local-ref element) result))) '() array))

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
)
