

#|

compute required board computations for z3 

corners done already

boolean variables from s_1_1 to s_10_10 , 100 of them

corner logic

side logic

|#


(defpackage :tc
  (:use :cl))
(in-package :tc)

;; do (in-package :tc)




(defun off-board-p (square)
  (let ((x (car square))
	(y (car (cdr square))))
    (cond
      ((< x 1) t)
      ((< y 1) t)
      ((> x 10) t)
      ((> y 10) t)
      (t nil))))



(defun on-board-p (square)
  (not (off-board-p square)))


(defun traverse ()
  (loop for x from 1 to 10 do
    (loop for y from 1 to 10 do
      (let ((neighbours (list (list (- x 1) y)
			      (list (+ x 1) y)
			      (list x (- y 1))
			      (list x (+ y 1)))))
	(let ((square (list x y))
	      (true-neighbours (remove-if-not #'on-board-p neighbours)))
	  (format t "true neighbours ~a => ~a ~%" square true-neighbours)

	  (let ((len (length true-neighbours)))
	    (cond
	      ((= len 2) (corner true-neighbours))
	      ((= len 3) (edge true-neighbours))
	      ((= len 4) (internal true-neighbours))
	      (t (error "cannot exist")))))))))


(defun corner () t)
(defun edge () t)
(defun internal () t)


	

	    

