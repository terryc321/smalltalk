#lang racket

(define cells-per-side 10)

(define neighbours
  (lambda (cell)
    (let ((x (car cell))
	  (y (car (cdr cell))))
      (let ((result '()))
	(when (> x 1) (set! result (cons (list (- x 1) y)  result)))
	(when (< x cells-per-side) (set! result (cons (list (+ x 1) y)  result)))
	(when (> y 1) (set! result (cons (list x (- y 1))  result)))
	(when (< y cells-per-side) (set! result (cons (list x (+ y 1))  result)))
	result))))

;; ???
(define next-list
  (lambda ()
    (let ((result '()))
      (letrec ((rec2 (lambda (n m k)
		      ;;(display (format "checking n(~a) : m(~a) : k(~a) ~%" n m k))
                       (set! result (cons (list n m) result))
		       (cond
			;;((> k cells-per-side) #f)
		        ((> n 1) (rec2 (- n 1) (+ m 1) k))
		        ((<= k cells-per-side) (rec2 k 1 (+ k 1) )))))
               (rec3 (lambda (n m k)
		      ;;(display (format "checking n(~a) : m(~a) : k(~a) ~%" n m k))
                       (set! result (cons (list n m) result))
		       (cond
			;;((> k cells-per-side) #f)
		        ((> m 1) (rec3 (+ n 1) (- m 1) k))
		        ((<= k cells-per-side) (rec3 1 k (+ k 1) ))))))
	;;(rec2 1 1 2)
        (rec3 1 cells-per-side 2)
	(reverse result)))))

(next-list)

#|

-100 to +100

R 1
1 1 k=2
-1 => k 1 k=3
2 1
1 2
-1 => k 2 k=4
3 1
2 2
1 3
-1 => k 3 k=5
...
10 1
9 2
8 3

|#

(define search
  (lambda (must cant next)
    (cond
      ((empty? next) #f)
      (#t (let ((opt (car next))) ;; pick first from next list to process
      	    (let ((nb (neighbours opt))) ;; get the neighbours
	      ...
	      ))))))


	      

