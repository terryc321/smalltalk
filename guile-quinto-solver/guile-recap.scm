;;; ***
;;; -*- guile scheme -*-  ***
;;; ***

#|

specifics related to guile scheme
we are


|#



;; ========= DEFMACRO - ABSOLUTELY need UN-HYGEINCIC MACROS =====
"x gets overwritten , so save it to tmp first "
(defmacro swap! (x y)
  (let ((tmp (gensym)))
  `(let ((,tmp ,x))
     (set! ,x ,y)
     (set! ,y ,tmp))))


;; C-c RET C-e  see guile macro expansion equivalent
(let ((a 1)(b 2))
  (swap! a b)
  (list a b))




;; ========= TEST SUITES need SRFI - 64 =============
(use-modules (srfi srfi-64))

;; Initialize and give a name to a simple testsuite.
(test-begin "vec-test")
(define v (make-vector 5 99))
;; REquuire expression eval to true but its false
(test-assert #f)

;; Require that an expression evaluate to true.
(test-assert (vector? v))
;; Test that an expression is eqv? to some other expression.
(test-eqv 99 (vector-ref v 2))
(vector-set! v 2 7)
(test-eqv 7 (vector-ref v 2))
;; Finish the testsuite, and report results.
(test-end "vec-test")
