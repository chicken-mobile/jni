(module jni-array
*
(import chicken scheme jni2-lolevel)

(define (array-fold procedure knil array)
  (let ((l (array-length array)))
    (let loop ((knil knil) (idx 0))
      (if (= idx l)
	  knil (loop (procedure (array-ref array idx) knil) (+ 1 idx))))))

(define (array-map procedure array)
  (reverse
   (array-fold 
    (lambda (element result)
      (cons (procedure element) result)) '() array)))

(define (array-find predicate array)
  (call/cc 
   (lambda (return)
     (array-fold
      (lambda (element result)
	(if (predicate element) 
	    (return element)
	    (delete-local-ref element))) '() array))))

(define (array-filter predicate array)
  (array-fold
   (lambda (element result)
     (if (predicate element) 
	 (cons element result)
	 (begin (delete-local-ref element) result))) '() array))

(define (array->list array)
  (array-map cons array))


(define (list->array class lst)
  (let ((arr (make-array (length lst) class #f)))
    (let loop ((i 0) (lst lst))
      (if (null? lst) arr
	  (begin
	    (array-set! arr i (car lst))
	    (loop (+ i 1) (cdr lst)))))))
)
