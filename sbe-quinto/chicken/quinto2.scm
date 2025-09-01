

;; #|
;; quinto 

;; can we reason a solution ?

;; are there other solutions - how would you find them ?

;; <model checkers>

;; chicken-install -s generalized-arrays
;; chicken-install -s test


;; <chicken scheme>

;; grid say 10 by 10 each square is either ON or OFF
;; all squares initially are OFF
;; we want final state to be all ON squares

;; matrix grid in scheme



;; |#

;; (import generalized-arrays)

;; (import (generalized-arrays intervals)
;;         (test))

;; (test "Length of [(0, 0) (2, 2)) is 4"
;;        4
;;        (make-default-interval (vector 2 2)))

;; (test "Length of [(1, 1) (3, 4)) is 6"
;;        6
;;        (make-interval (vector 1 1)
;;                       (vector 3 4)))

;; (test "divide by zero is fine"
;;        0
;;        (/ 3 0))

;; (test "divide 3 by 1 is 3"
;;        3
;;        (/ 3 1))


;; (define arr (make-interval (vector 1 1) (vector 2 2)))

;; ;;(arr 1 1)


;; (test-assert "Array is filled with #f"
;;   (array=? #a2v((#f #f #f)
;;                 (#f #f #f)
;;                 (#f #f #f))
;;            (make-array vector-storage-class #(3 3) #f)))


;; (array=? #a2v((1 2 3)
;; 	      (4 5 6)
;; 	      (7 8 9)) #t)


;; (array? #a2v((1 2 3)
;; 	      (4 5 6)
;; 	      (7 8 9)))


;; ;; (define arr #a2v((1 2 3)
;; ;; 		 (4 5 6)
;; ;; 		 (7 8 9)))


;; ;; arr


;; ;; (vector-ref arr 1)

;; ;; (array-ref 1 1)

;; ;; (array-ref arr '(1 1))

;; ;; (array? arr)
;; ;; (array-ref

;; (import (srfi-179))

;; (define a (make-array (make-interval '#(1 1) '#(10 10))
;;                       (lambda (i j)
;; 			(list i j (+ i j)))))


;; (array? a)
;; (array-ref a 1 1)
;; (array-ref a 10 10)

;; (array-ref a 0 0)
;; (array-ref a -1 -1)

;; (array-set! a 1 1 11)

