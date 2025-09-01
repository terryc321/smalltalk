
;;; ***
;;; -*- guile scheme -*-  ***
;;; ***

#|

specifics related to guile scheme
we are


|#
(use-modules (srfi srfi-1)) ;; first second third 
(use-modules (ice-9 pretty-print)) ;; pp pretty printer
(use-modules (ice-9 format))  ;; format 
;;(use-modules (ice-9 assert))	no support for assertions?
(use-modules (srfi srfi-64)) ;; for test suites



		  




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

;; Initialize and give a name to a simple testsuite.
(test-begin "vec-test")
(define v (make-vector 5 99))
;; REquuire expression eval to true but its false
;;(test-assert #f)

;; Require that an expression evaluate to true.
(test-assert (vector? v))
;; Test that an expression is eqv? to some other expression.
(test-eqv 99 (vector-ref v 2))
(vector-set! v 2 7)
(test-eqv 7 (vector-ref v 2))
;; Finish the testsuite, and report results.
(test-end "vec-test")



#|

quinto solver 

10 x 10 grid = 100 squares

if one click eliminates 4 squares

optimal version = 25 steps
although i think probably a few more to cater for corners only remove 2 squares

have solved it - using smalltalk gui and clicking buttons - but gui maybe susceptible errors

x o
o

in corner click x - takes out two O's
2d grid ?

how do we get chicken docs info again ? i forgot ?

|#

(define cells-per-side 10)

(define cannot-press '())
(define must-press '())

#|
for each square in a more or less a diagonal pattern slowly reaching outward
since closer we remain to squares we know about , the earlier we will reach a logical
contradiction , which will cut search space dramatically
sooner we fail , quicker we stay on correct search path

(1 1) means square 1 1
if (1 1) is in cannot-press list , we cannot press it

|#

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

#|
(test "neighbours of cell 1 1 should be 1 2 and 2 1"
      '((1 2)(2 1))
      (neighbours '(1 1)))

;;(neighbours '(1 1))
(neighbours '(1 2))
(neighbours '(1 3))
(neighbours '(10 10))
(neighbours '(9 10))

(test "neighbours of cell 1 2 should be 1 1 and 2 2 and 1 3"
      '((1 3)(1 1)(2 2))
      (neighbours '(1 2)))
|#


#|
each cell must only have an odd number of pressed neighbours , when this constraint
fails , the search is futile , since no solution can be found this way

must - those cells that must be pressed
cannot - those cells that cannot be pressed
next - list of cells to consider in specific order those cells closest to known cells

|#

(define next-list
  (lambda ()
    (let ((result '()))
      (letrec ((rec2 (lambda (n m k)
		       ;;(format #t "checking n(~a) : m(~a) : k(~a) ~%" n m k)
		       (set! result (cons (list n m) result))
		       (cond
			((> k cells-per-side) #f)
		        ((> n 1) (rec2 (- n 1) (+ m 1) k))
		        ((<= m cells-per-side) (rec2 k 1 (+ k 1) ))))))		      
	(rec2 1 1 1)
	(set! result (reverse result))
	result))))




;;(next-list)

#|
opt chosen square to examine

do want do consistency check over all board - to ensure never miss opportunity to prune
search tree ?

given opt - find neighbours

square is undecided if neither in must list OR the cannot list
all squares are by default undecided at start up

ask question on each square given information i know about what squares must be pressed
what squares cannot be pressed
leaves with a selection of what squares can be pressed

each square given neighbours = 4 must pick 1 or 3 neighbours to be definitively pressed,
other 3 or 1 neighbour is then definitively rejected

corner square has 2 neighbours = only one of those neighbours can be definitively selected
rather one neighbour must be selected , other neighbour must be rejected.

edge square - 3 neighbours = one neighbour selected or three neighbours selected


|#

(define corner?
  (lambda (cell)
    (let ((x (car cell))
	  (y (car (cdr cell))))
      (cond
       ((and (= x 1) (= y 1)) #t)
       ((and (= x cells-per-side) (= y 1)) #t)
       ((and (= x 1) (= y cells-per-side)) #t)
       ((and (= x cells-per-side) (= y cells-per-side)) #t)       
       (#t #f)))))

#|
(test "corner? 1 1"       #t      (corner? '(1 1)))
(test "corner? 1 10"      #t      (corner? '(1 10)))
(test "corner? 10 1"      #t      (corner? '(10 1)))
(test "corner? 10 10"     #t      (corner? '(10 10)))
|#

(define edge?
  (lambda (cell)
    (let ((x (car cell))
	  (y (car (cdr cell))))
      (cond
       ((and (= x 1) (>= y 2) (<= y cells-per-side)) #t)
       ((and (= x cells-per-side) (>= y 2) (<= y cells-per-side)) #t)
       ((and (= y 1) (>= x 2) (<= x cells-per-side)) #t)
       ((and (= y cells-per-side) (>= x 2) (<= x cells-per-side)) #t)
       (#t #f)))))

#|
(test "edge? 1 1"      #f      (edge? '(1 1)))

(test "edge? 2 1"      #t      (edge? '(2 1)))
(test "edge? 3 1"      #t      (edge? '(3 1)))
(test "edge? 4 1"      #t      (edge? '(4 1)))
(test "edge? 5 1"      #t      (edge? '(5 1)))
(test "edge? 6 1"      #t      (edge? '(6 1)))
(test "edge? 7 1"      #t      (edge? '(7 1)))
(test "edge? 8 1"      #t      (edge? '(8 1)))
(test "edge? 9 1"      #t      (edge? '(9 1)))

(test "edge? 1 2"      #t      (edge? '(1 2)))
(test "edge? 1 2"      #t      (edge? '(1 2)))
(test "edge? 1 2"      #t      (edge? '(1 2)))
(test "edge? 1 2"      #t      (edge? '(1 2)))
(test "edge? 1 2"      #t      (edge? '(1 2)))
(test "edge? 1 2"      #t      (edge? '(1 2)))
(test "edge? 1 2"      #t      (edge? '(1 2)))
(test "edge? 1 2"      #t      (edge? '(1 2)))
|#

(define inside?
  (lambda (cell)
    (cond
     ((corner? cell) #f)
     ((edge? cell) #f)
     (#t #t))))

#|
(test "inside? 1 1"      #f  (inside? '(1 1)))
(test "inside? 4 1"      #f  (inside? '(4 1)))
(test "inside? 2 2"      #t  (inside? '(2 2)))
|#




;; ========== Macro definitoins first ================

;;(inside-214-macro)

(defmacro inside-214-macro ()
  `(when (and (= len-m 2)(= len-c 1)(= len-nb 4)
    ;;  2    1     4  ->  
    (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	   (choices (filter (lambda (cell) (not (member cell must))) without-cant)))

      (let ((p1 (first choices)))
	;; pick p1 , only choice to make to hold constraint 1 or 3 selected neighbours must.
	(search3 (cons p1 must) cannot (cdr next))
	)))))



;;(inside-214-macro)



(defmacro inside-204-macro ()
  `(when (and (= len-m 2)(= len-c 0)(= len-nb 4))
    ;;  2    1     4  ->  
    (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	   (choices (filter (lambda (cell) (not (member cell must))) without-cant)))

      (let ((p1 (first choices)))
	;; pick p1 , only choice to make to hold constraint 1 or 3 selected neighbours must.
	(search3 (cons p1 must) cannot (cdr next))
	))))



(defmacro inside-124-macro ()
  `(when (and (= len-m 1)(= len-c 2)(= len-nb 4))
    ;;  1    2     4  ->  nothing to do since choices should be length 1 that would unbalance
    (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	   (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
      (cond
       ((= (length choices) 1)
	(search3 must cannot (cdr next)))
       (#t
	(format #t "MCB124 : errro cannot have choices length differ from 1 ? surely.~%")
	(error "absurdity"))))))



(define search3
  (lambda (must cannot next)
    (cond
     ((null? next) must)
     (#t (let ((opt (car next)))
	   ;; continue search with next as (cdr next)
	   (let ((nb (neighbours opt)))
	     (let* ((m (filter (lambda (cell) (member cell must)) nb))
		    (c (filter (lambda (cell) (member cell cannot)) nb))
		    (len-m (length m))
		    (len-c (length c))
		    (len-nb (length nb)))
	       ;; m c nb
	       ;; local must , local cannot , local neighbours
	       (format #t "~%~%considering ~a~%" opt)
	       (format #t "m(~a) ~a : must ~a~%" len-m m must)
	       (format #t "c(~a) ~a : cannot ~a ~%" len-c c cannot)
	       (format #t "neigh(~a) ~a ~%" len-nb nb)
	       
	       (cond
		((corner? opt)
		 ;; ================ CORNER CASES ======================
		 (format #t "corner square !~%")
		 ;; cases
		 ;; must cannot neighbour
		 ;;  0    0     2  -> pick one neighbour as accept, other reject
		 ;;  0    1     2  -> pick one neighbour not in cannot, other already in cant
		 ;;  0    2     2  -> contradict
		 ;;  1    0     2  -> fully defined ,no action , move next square
		 ;;  1    1     2  -> fully defined
		 ;;  1    2     2  -> absurd should not happen !
		 ;;  2    0     2  -> absurd
		 ;;  2    1     2  -> absurd
		 ;;  2    2     2  -> absurd		 
		 (cond
		  ((and (= len-m 0) (= len-c 0) (= len-nb 2))
		   (format #t "corner case 1 - free pick one neighbour, reject other ~%")
		   (let ((p1 (car nb))
			 (p2 (car (cdr nb))))
		     ;; pick first
		     (search3 (cons p1 must) (cons p2 cannot) (cdr next))
		     ;; pick second
		     (search3 (cons p2 must) (cons p1 cannot) (cdr next)))
		   ) ;; m0 c0 nb2
		  
		  (#t (error "corner case not handled")))
		 ;; assert length nb == 2
		 ) ;; end corner
		((edge? opt)
		 ;; ================ EDGE  CASES ======================
		 (format #t "edge square ")
		 ;; cases 
		 ;; must cannot neighbour
		 (cond
		  ((and (= len-m 0)(= len-c 0)(= len-nb 3))
		   ;;  0    0     3  -> pick 1 neighbour,reject 2
		   (let ((p1 (first nb))
			 (p2 (second nb))
			 (p3 (third nb)))
		     ;; pick 1 neighbour reject other 2 neighbours
		     (search3 (cons p1 must) (append (list p2 p3) cannot) (cdr next))
		     (search3 (cons p2 must) (append (list p1 p3) cannot) (cdr next))
		     (search3 (cons p3 must) (append (list p1 p2) cannot) (cdr next))
		     ;;pick all 3 neighbours
		     (search3 (append (list p1 p2 p3) must) cannot (cdr next))
		     )) ;;end of  0    0     3  -> pick all 3 neighbours

		  ((and (= len-m 0)(= len-c 1)(= len-nb 3))
		   ;;  0    1     3  ->  1 square cannot
		   (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
			  (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
		     ;; currently no neighbours selected must
		     ;; must be pick 1 neighbour , cannot pick 3 since only 3 choose and 1 is already rejected
		     (let ((p1 (first choices))
			   (p2 (second choices)))

		       ;; pick p1 , rject p2
		       (search3 (cons p1 must) (cons p2 cannot) (cdr next))
		       ;; pick p2 , rject p1
		       (search3 (cons p2 must) (cons p1 cannot) (cdr next))		       
		       ;; done
		       ))
		   );; end of 0 1 3

		  ((and (= len-m 0)(= len-c 2)(= len-nb 3))
		   ;;  0 2 3  ->  
		   (format #t "EDGE MCB023 : hello fox .~%")
		   (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
			  (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
		     (cond
		      ((= (length choices) 1)
		       ;; only thing to do is add first choice
		       (let ((p1 (first choices)))
		       ;; pick p1 
			 (search3 (cons p1 must) cannot (cdr next))))
		      (#t
		       (format #t "MCB023 : errro cannot have choices length differ from 1 ? surely.~%")
		       (error "absurdity"))))
		   );; end of 023

		  

		  ((and (= len-m 1)(= len-c 0)(= len-nb 3))
		   ;;  1 0 3  ->  do nothing ok , or select other two also 
		   (format #t "EDGE MCB103 : hello .~%")
		   (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
			  (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
		     (cond
		      ((= (length choices) 2)
		       ;; nothing added must list , choices avail are placed in rejected
		       (search3 must (append choices cannot) (cdr next))
		       ;; add choices onto must list , reject list untouched ,
		       (search3 (append choices must) cannot (cdr next)))
		      (#t
		       (format #t "MCB103 : errro cannot have choices length differ from 2 ? surely.~%")
		       (error "absurdity"))))
		   );; end of 103
		  
		  
		  ;;  0    2     3  ->
		  ;;  0    3     3  ->
		  ;;  0    1     3  ->
		  ;;  1    1     3  ->
		  ((and (= len-m 1)(= len-c 1)(= len-nb 3))
		   ;; 1 neighbour already must selected , 1 cannot be selected ,
		   ;; nothing to do but continue without selection because if select last
		   ;; neighbour as must , have 2 neigbours selected violates constraint only 1 or 3
		   ;; neighbours selected as must.
		   (search3 must cannot (cdr next))
		  );; end of 1 1 3 case 
		  ;;  1    2     3  ->
		  ;;  1    3     3  ->
		  ;;  2    0     3  ->
		  ((and (= len-m 2)(= len-c 0)(= len-nb 3))
		   (format #t "edge case MCB203: debug help len-m-c-nb ~a ~a ~a~%" len-m len-c len-nb)
		   ;; 2 neighbours must . no neighbours in cannot , 3 neighbours in total
		   ;; must choose only remaining
		   (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
			  (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
		     (format #t "choices =~a~%" choices)
		     (let ((p1 (first choices)))
		       ;; pick p1 
		       (search3 (cons p1 must) cannot (cdr next))
		       ))
		   
		  );; end of 2 0 3 case 
		  
		  ;;  2    1     3  ->
		  ;;  2    2     3  ->
		  ;;  2    3     3  ->
		  ;;  3    0     3  ->
		  ;;  3    1     3  ->
		  ;;  3    2     3  ->
		  ;;  3    3     3  ->
		  ;; assert length nb == 3
		  (#t
		   (format #t "edge case : debug help len-m-c-nb ~a ~a ~a~%" len-m len-c len-nb)
		   (error "edge case not handled!")))
		 );; end edge
		((inside? opt)
		 ;; ================ INSIDE CASES ======================
		 (format #t "inside square ")

		 (let ((handled #f))
		  
		  (inside-214-macro)
		  
		  ;; ((and (= len-m 2) (= len-c 1) (= len-nb 4))
		  ;;  (let* ((without-cant
		  ;; 	   (filter
		  ;; 	    (lambda (cell) (not (member cell cannot)))
		  ;; 	    nb))
		  ;; 	  (choices
		  ;; 	   (filter
		  ;; 	    (lambda (cell) (not (member cell must)))
		  ;; 	    without-cant))
		  ;; 	  (p1 (first choices)))
		  ;;    (search3 (cons p1 must) cannot (cdr next))))

		  
		  ;;(inside-124-macro)
		  
		 ;; assert length nb == 4 
		 ;; 4 in must-nb -> contradiction , do not continue!
		 ;; 3 in must-nb -> fully defined , no action , move next square
		 ;; caveat
		 ;; 2 in must-nb 
		 ;; 1 in must-nb 
		  ;; 0 in must-nb 0 1 2 3 4 in cannot-nb

		  (when (not handled)
		    (format #t "inside case : debug help len-m-c-nb ~a ~a ~a~%"
			    len-m len-c len-nb)
		   (error "inside case not handled!"))
		  
		 ) ;; handled flag cond
		 )))))))))

  











;; (cond
;; 	((and (null? must-nb)(null? cannot-mb))
;; 	 (let ((take1nb (pick1 nb))
;; 	       (take3nb (pick3 nb)))
;; 	   (let ((len1 (length take1nb))
;; 		 (len3 (length take3nb)))
;; 	   (cond
;; 	    ((= len1 1) (error "not implemented yet"))
;; 	    ((= len3 3) (error "not implemented yet"))))))))))))))






(define run
  (lambda ()
    (let ((must '())
	  (cannot '())
	  (next '((1 1)
		  (2 1)(1 2)
		  (3 1)(2 2)(1 3)
		  (4 1)(3 2)(2 3)(1 4)
		  (5 1)(4 2)(3 3)(2 4)(1 5)
		  (6 1)(5 2)(4 3)(3 4)(2 5)(1 6)
		  (7 1)(6 2)(5 3)(4 4)(3 5)(2 6)(1 7)
		  (8 1)(7 2)(6 3)(5 4)(4 5)(3 6)(2 7)(1 8)
		  (9 1)(8 2)(7 3)(6 4)(5 5)(4 6)(3 7)(2 8)(1 9)
		  (10 1)(9 2)(8 3)(7 4)(6 5)(5 6)(4 7)(3 8)(2 9)(1 10)
		  ;; still some remaining
		  
		  )))
      (search3 must cannot next))))






;; helper turn list into smalltalk message
(define must->smalltalk
  (lambda (xs)
    (format #t "~%s setMoves: {")
    (map (lambda (pr)
	   (format #t " ~a~a~a ." (first pr) "@" (second pr))) xs)
    (format #t "}.~%")))

(must->smalltalk '((3 3) (4 2) (1 4) (2 3) (3 2) (2 2) (1 2)))










