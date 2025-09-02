;; define quinto board in propositional logic
;; we want to find what squares we should press to turn entire board on
;; if we ignore actual end state of squares and simply state what squares must be pressed
;; in order for the outcome to occur we want , a necessary condition for solution to be
;; possible


;; top left square

;; squares    their positions 
;; o y _       [1,1] [2,1] [3,1] ...
;; x _ _       [1,2] [2,2] [3,2] ...
;; _ _ _       [1,3] [2,3] [3,3] ...

;; think about way to classify squares

;; must squares - are squares must be pressed
;; cannot squares - are squares that cannot be pressed
;; since must and cannot are mutually exclusive a single boolean variable can be used
;; to represent both cases

;; s0101 to mean square at 1 1 is to be pressed
;; -s0101 the negation of s0101 to mean square 1 1 is not to be pressed - ie left alone

;; consider top left square again we see that the 'x' square or the 'y' square
;; must be pressed
;; if both are pressed the 'o' square will be OFF , and that is not what we want

;; so either 'x' pressed 'y' not pressed
;; OR 'x' not pressed 'y' is pressed
;; no other cases can be correct

;; little step in thinking to move away from if a square is ON or OFF ,
;; to specify what must be true in order for the consequence of the square to be ON or OFF

;; really , what is happening under the rug to a certain extent that will lead us to a
;; position to say that a certain configuration of pressed buttons CAN or CANNOT lead us to
;; a solution

; top left portion of quinto in logic
; top left square is on if xor square below on , square right is off

(declare-const s_1_1 Bool) ;; 
(declare-const s_2_1 Bool)
(declare-const s_3_1 Bool)
(declare-const s_4_1 Bool)
(declare-const s_5_1 Bool)
(declare-const s_6_1 Bool)
(declare-const s_7_1 Bool)
(declare-const s_8_1 Bool)
(declare-const s_9_1 Bool)
(declare-const s_10_1 Bool) ;; [10,1] 1st row

(declare-const s_1_2 Bool) ;; 
(declare-const s_2_2 Bool)
(declare-const s_3_2 Bool)
(declare-const s_4_2 Bool)
(declare-const s_5_2 Bool)
(declare-const s_6_2 Bool)
(declare-const s_7_2 Bool)
(declare-const s_8_2 Bool)
(declare-const s_9_2 Bool)
(declare-const s_10_2 Bool) ;; [10,2] 2nd row

(declare-const s_1_3 Bool) ;; 
(declare-const s_2_3 Bool)
(declare-const s_3_3 Bool)
(declare-const s_4_3 Bool)
(declare-const s_5_3 Bool)
(declare-const s_6_3 Bool)
(declare-const s_7_3 Bool)
(declare-const s_8_3 Bool)
(declare-const s_9_3 Bool)
(declare-const s_10_3 Bool) ;; [10,3] 3rd row

(declare-const s_1_4 Bool) ;; 
(declare-const s_2_4 Bool)
(declare-const s_3_4 Bool)
(declare-const s_4_4 Bool)
(declare-const s_5_4 Bool)
(declare-const s_6_4 Bool)
(declare-const s_7_4 Bool)
(declare-const s_8_4 Bool)
(declare-const s_9_4 Bool)
(declare-const s_10_4 Bool) ;; [10,4] 4rd row

(declare-const s_1_5 Bool) ;; 
(declare-const s_2_5 Bool)
(declare-const s_3_5 Bool)
(declare-const s_4_5 Bool)
(declare-const s_5_5 Bool)
(declare-const s_6_5 Bool)
(declare-const s_7_5 Bool)
(declare-const s_8_5 Bool)
(declare-const s_9_5 Bool)
(declare-const s_10_5 Bool) ;; [10,5] 5th row

(declare-const s_1_6 Bool) ;; 
(declare-const s_2_6 Bool)
(declare-const s_3_6 Bool)
(declare-const s_4_6 Bool)
(declare-const s_5_6 Bool)
(declare-const s_6_6 Bool)
(declare-const s_7_6 Bool)
(declare-const s_8_6 Bool)
(declare-const s_9_6 Bool)
(declare-const s_10_6 Bool) ;; [10,6] 6th row

(declare-const s_1_7 Bool) ;; 
(declare-const s_2_7 Bool)
(declare-const s_3_7 Bool)
(declare-const s_4_7 Bool)
(declare-const s_5_7 Bool)
(declare-const s_6_7 Bool)
(declare-const s_7_7 Bool)
(declare-const s_8_7 Bool)
(declare-const s_9_7 Bool)
(declare-const s_10_7 Bool) ;; [10,7] 7th row

(declare-const s_1_8 Bool) ;; 
(declare-const s_2_8 Bool)
(declare-const s_3_8 Bool)
(declare-const s_4_8 Bool)
(declare-const s_5_8 Bool)
(declare-const s_6_8 Bool)
(declare-const s_7_8 Bool)
(declare-const s_8_8 Bool)
(declare-const s_9_8 Bool)
(declare-const s_10_8 Bool) ;; [10,8] 8th row

(declare-const s_1_9 Bool) ;; 
(declare-const s_2_9 Bool)
(declare-const s_3_9 Bool)
(declare-const s_4_9 Bool)
(declare-const s_5_9 Bool)
(declare-const s_6_9 Bool)
(declare-const s_7_9 Bool)
(declare-const s_8_9 Bool)
(declare-const s_9_9 Bool)
(declare-const s_10_9 Bool) ;; [10,9] 9th row

(declare-const s_1_10 Bool) ;; 
(declare-const s_2_10 Bool)
(declare-const s_3_10 Bool)
(declare-const s_4_10 Bool)
(declare-const s_5_10 Bool)
(declare-const s_6_10 Bool)
(declare-const s_7_10 Bool)
(declare-const s_8_10 Bool)
(declare-const s_9_10 Bool)
(declare-const s_10_10 Bool) ;; [10,10] 10th row



;;(declare-const s0201 Bool)
;; (declare-const r Bool)


;; https://www.philipzucker.com/z3-rise4fun/guide.html
;; see propositional logic
;;
;; (=> x y) means implies
;; (not x)
;; (or x y)
;; (xor x y)

;; comment
;; (define-fun conjecture () Bool
;; 	(=> (and (=> p q) (=> q r))
;; 	    (=> p r)))

;;  (define-fun conjecture () Bool
;;  	    (s0101 => s0102))

;;(assert s0101)

;; corner squares
(assert (xor s_2_1  s_1_2)) ;; upper left corner
(assert (xor s_9_1  s_10_2)) ;; upper right corner
(assert (xor s_1_9  s_2_10)) ;; lower left corner
(assert (xor s_10_9 s_9_10)) ;; lower right corner

;;      s11   (s21)  s31
;;             s22
;;

;; can it do multiple xors
(assert (xor (and s_1_1 (not s_3_1) (not s_2_2))
	     (and s_3_1 (not s_1_1) (not s_2_2))
	     (and s_2_2 (not s_1_1) (not s_3_1))
	     (and s_1_1 s_3_1 s_2_2)))

;;


;; (assert (not conjecture))
(check-sat)

;; get a solution
(get-model)


