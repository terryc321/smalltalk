
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
(use-modules (srfi srfi-13)) ;; for hash tables

;; ===== hash table refresher ====
;; (define h (make-hash-table))
;; (hash-ref h 'a 'nope)
;; (hash-set! h 'a 'hihi)

;; shortcut for print
;; to enable debug messages use (format #t ,@args)
(defmacro debug (tf . args)
  `(format #f ,@args))
  





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
		       ;;(debug #t "checking n(~a) : m(~a) : k(~a) ~%" n m k)
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

;; LANGFIX - disallow macro redefinition ?
(defmacro inside-034-macro ()
  `(when (and (= len-m 0)(= len-c 3)(= len-nb 4))
     (set! handled #t)
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       ;;
       ;; 0 3 4 -> 0 must . 3 cannot . 4 tot 
       ;; pick 1 avaiablle choice add to must
       ;;
       ;; TODO - assert choices len = 1 
       (let ((p1 (first choices)))
	 ;; pick p1 , only choice to make to hold constraint 1 or 3 selected neighbours must.
	 (search3 (cons p1 must) cannot (cdr next))
	 ))))

(defmacro inside-014-macro ()
  `(when (and (= len-m 0)(= len-c 1)(= len-nb 4))
     ;;
     (set! handled #t)
     ;; 0 1 4 -> 0 must . 1 cannot . 4 total . 3 choices
     ;; decision 1 - add 1 choice to must , add other 2 to cannot , continue
     ;; decision 2 - add 3 choice to must , cannot untouched , continue
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))

       ;; assert choices is len 3 
       (let ((p1 (first choices))
	     (p2 (second choices))
	     (p3 (third choices)))
	 ;; decision 1
	 (search3 (cons p1 must) (append (list p2 p3) cannot) (cdr next))
	 (search3 (cons p2 must) (append (list p1 p3) cannot) (cdr next))
	 (search3 (cons p3 must) (append (list p1 p2) cannot) (cdr next))
	 ;; decision 2
	 (search3 (append (list p1 p2 p3) must) cannot (cdr next))))))



(defmacro inside-214-macro ()
  `(when (and (= len-m 2)(= len-c 1)(= len-nb 4))
     (set! handled #t)
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))

       (let ((p1 (first choices)))
	 ;; pick p1 , only choice to make to hold constraint 1 or 3 selected neighbours must.
	 (search3 (cons p1 must) cannot (cdr next))
	 ))))

(defmacro inside-204-macro ()
  `(when (and (= len-m 2)(= len-c 0)(= len-nb 4))
     (set! handled #t)
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))

       (let ((p1 (first choices)))
	 ;; pick p1 , only choice to make to hold constraint 1 or 3 selected neighbours must.
	 (search3 (cons p1 must) cannot (cdr next))
	 ))))

(defmacro inside-124-macro ()
  `(when (and (= len-m 1)(= len-c 2)(= len-nb 4))
     ;;  1    2     4  ->  nothing to do since choices should be length 1 that would unbalance
     (set! handled #t)
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       (cond
	((= (length choices) 1)
	 (search3 must cannot (cdr next)))
	(#t
	 (format #t "MCB124 : errro cannot have choices length differ from 1 ? surely.~%")
	 (error "absurdity"))))))


(defmacro inside-304-macro () 
  `(when (and (= len-m 3)(= len-c 0)(= len-nb 4))
     ;;  3 0 4 ->
     (set! handled #t)
     ;; FIXED - thought because fully specified did not need to put only other choice
     ;; into the sin bin CANNOT. - FIXED now.
     ;;
     ;; 3 squares are labelled must. nowhere cannot go.
     ;; put only pick into cannot go - as fully specified 
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       (cond
	((= (length choices) 1)
	 (let ((p1 (first choices)))
	   (search3 must (cons p1 cannot) (cdr next))))
	(#t
	 (format #t "MCB304 : errro cannot have choices length differ from 1 ? surely.~%")
	 (error "absurdity"))))))



(defmacro inside-114-macro () 
  `(when (and (= len-m 1)(= len-c 1)(= len-nb 4))
     ;;  1 1 4 ->
     (set! handled #t)
     ;; 1 square must . 1 square cannot . 2 choices open
     ;; decision 1 - take as is , put 2 other choices into cannot bin
     ;; decision 2 - add 2 choices into must bin . cannot bin untouched.     
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       (cond
	((= (length choices) 2)
	 (let ((p1 (first choices))
	       (p2 (second choices)))
	   ;; decision 1
	   (search3 must (append (list p1 p2) cannot) (cdr next))
	   ;; decision 2
	   (search3 (append (list p1 p2) must) cannot (cdr next))))
	(#t
	 (format #t "MCB114 : errro cannot have choices length differ from 1 ? surely.~%")
	 (error "absurdity"))))))


(defmacro inside-024-macro () 
  `(when (and (= len-m 0)(= len-c 2)(= len-nb 4))
     ;;  0 2 4 ->
     (set! handled #t)
     ;; 0 square must . 2 square cannot . 2 choices open
     ;; decision 1 - put 1 into must, other into cannot
     ;; decision 2 - put 1 into must , other into cannot
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       (cond
	((= (length choices) 2)
	 (let ((p1 (first choices))
	       (p2 (second choices)))
	   ;; decision 1
	   (search3 (cons p1 must) (cons p2 cannot)  (cdr next))
	   ;; decision 2
	   (search3 (cons p2 must) (cons p1 cannot) (cdr next))))
	(#t
	 (format #t "MCB024 : errro cannot have choices length differ from 2 ? surely.~%")
	 (error "absurdity"))))))








(defmacro inside-104-macro ()
  `(when (and (= len-m 1)(= len-c 0)(= len-nb 4))
     ;;  1 0 4 ->
     (set! handled #t)
     ;; 1 square labelled must . nowhere cannot go. 
     ;; 
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       (cond
	((= (length choices) 1)
	 (search3 must cannot (cdr next)))
	(#t
	 (format #t "MCB304 : errro cannot have choices length differ from 1 ? surely.~%")
	 (error "absurdity"))))))


;; ============= Corner cases macros ==================
(defmacro corner-202-macro ()
  `(when (and (= len-m 2) (= len-c 0) (= len-nb 2))
     ;;
     (when (eq? handled #t) (format #t "possible duplicate impl corner-002~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;; 2 0 2 -> 2 must 0 cannot 2 total
     ;; contradiction reached - do not proceed
     ))

(defmacro corner-102-macro ()
  `(when (and (= len-m 1) (= len-c 0) (= len-nb 2))
     ;; LANGFIX - out date/out sync error messages
     (when (eq? handled #t) (format #t "possible duplicate impl corner-102~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;;
     ;; 1 0 2 -> 1 must . 0 cannot . 2 total.
     ;; put other choice into cannot bin .
     ;;
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))

       ;; assert choices len 1 
       (let ((p1 (first choices)))
	 (search3 must (cons p1 cannot) (cdr next))))))

(defmacro corner-022-macro ()
  `(when (and (= len-m 0) (= len-c 2) (= len-nb 2))
     ;; LANGFIX - out date/out sync error messages
     (when (eq? handled #t) (format #t "possible duplicate impl corner-022~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;;
     ;; 0 2 2 -> 0 must . 2 cannot . 2 total.
     ;; contradition - do not continue
     ))



(defmacro corner-002-macro ()
  `(when (and (= len-m 0) (= len-c 0) (= len-nb 2))
     ;; handled should be #f , if its true - it means another case has fired already
     ;; expected only one situation applies to any particular combination
     ;; because they should be mutually exclusive
     (when (eq? handled #t) (format #t "possible duplicate impl corner-002~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;; tell user whats happenening - should be debug message instead
     (debug #t "corner case 1 - free pick one neighbour, reject other ~%")
     ;; corner case m0 c0 nb2
     ;; 
     (let ((p1 (car nb))
	   (p2 (car (cdr nb))))
       ;; pick first
       (search3 (cons p1 must) (cons p2 cannot) (cdr next))
       ;; pick second
       (search3 (cons p2 must) (cons p1 cannot) (cdr next)))
     ) ;; m0 c0 nb2
  )


(defmacro corner-012-macro ()
  `(when (and (= len-m 0) (= len-c 1) (= len-nb 2))
     ;; 
     (when (eq? handled #t) (format #t "possible duplicate impl corner-002~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;;
     ;; (debug #t "MCB-CORNER-012: { cannot == ~a } ~%" cannot)
     ;; 0 1 2 -> 0 must . 1 cannot . 2 total its a corner
     ;; add only remaining choice to must , nothing done to cannot bin , continue
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       ;; TODO - we do not assert choices list of length 1 
       (let ((p1 (first choices)))
	 ;; pick first
	 (search3 (cons p1 must) cannot (cdr next))))))


(defmacro corner-112-macro ()
  `(when (and (= len-m 1) (= len-c 1) (= len-nb 2))
     ;; LANGFIX - procedure/macro name insertion to debug messages
     (when (eq? handled #t) (format #t "possible duplicate impl corner-112~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;;
     ;; 1 1 2 -> 1 must . 1 forbidden. 2 total . should be no choices .
     ;;
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       ;; TODO - we do not assert choices list of length 0 
       (search3 must cannot (cdr next)))))

;; IDEFIX - multiple emacs windows - M-x set-background-color


;; ============= Edge cases macros =====================================

;; FIXED - EDGES some macros missing handled flag handling!! missing the 

;; (defmacro EDGES-macro ()
;;   `(when CONDITION
;;      ;; handled should be #f , if its true - it means another case has fired already
;;      ;; expected only one situation applies to any particular combination
;;      ;; because they should be mutually exclusive
;;      (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
;;      ;; set handled flag
;;      (set! handled #t)
;;      ;; tell user whats happenening - should be debug message instead
;;      BODY-GOES-HERE
;;      ))


(defmacro edge-003-macro ()
  `(when 
       (and (= len-m 0)(= len-c 0)(= len-nb 3))
     ;;  0    0     3  -> pick 1 neighbour,reject 2
     ;; handled should be #f , if its true - it means another case has fired already
     ;; expected only one situation applies to any particular combination
     ;; because they should be mutually exclusive
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;; tell user whats happenening - should be debug message instead
     (let ((p1 (first nb)) ;; TODO - WHY we using nb , should be CHOICES ...
	   (p2 (second nb))
	   (p3 (third nb)))
       ;; pick 1 neighbour reject other 2 neighbours
       (search3 (cons p1 must) (append (list p2 p3) cannot) (cdr next))
       (search3 (cons p2 must) (append (list p1 p3) cannot) (cdr next))
       (search3 (cons p3 must) (append (list p1 p2) cannot) (cdr next))
       ;;pick all 3 neighbours
       (search3 (append (list p1 p2 p3) must) cannot (cdr next)))))



(defmacro edge-123-macro ()
  `(when
       (and (= len-m 1)(= len-c 2)(= len-nb 3))
     ;;
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;; 1 2 3 -> 1 must . 2 cannot . 3 total . 
     ;; nothing to do , all good , just continue.
     ;; 
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       ;; assert choices len 0
       (search3 must cannot (cdr next)))))




(defmacro edge-303-macro ()
  `(when 
       (and (= len-m 3)(= len-c 0)(= len-nb 3))
     ;;
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;; 3 0 3 -> 3 must . 0 cannot . 3 total . 
     ;; nothing to do , all good , just continue.
     ;; 
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       ;; assert choices len 0
       (search3 must cannot (cdr next)))))

(defmacro edge-033-macro ()
  `(when 
       (and (= len-m 0)(= len-c 3)(= len-nb 3))
     ;;
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;; 0 3 3 -> 0 must . 3 cannot . 3 total
     ;; contradiction - do not continue
     ))


(defmacro edge-013-macro ()
  `(when 
       (and (= len-m 0)(= len-c 1)(= len-nb 3))
     ;;  0    1     3  ->  1 square cannot
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)

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
	 ))))

(defmacro edge-213-macro ()
  `(when 
       (and (= len-m 2)(= len-c 1)(= len-nb 3))
     ;;  2    1     3  ->  
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;; 2 1 3 -> 2 must 1 cannot 3
     ;; 2 neighbour squares must be selected , 1 cannot be chosen , only 3 total
     ;; no solutions follow from here
     ;; contradiction - do not continue
     ))




(defmacro edge-023-macro ()
  `(when 
       (and (= len-m 0)(= len-c 2)(= len-nb 3))
     ;;  0 2 3  ->
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)

     (debug #t "EDGE MCB023 : hello fox .~%")
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
	 (error "absurdity"))))))


(defmacro edge-103-macro ()
  `(when 
       (and (= len-m 1)(= len-c 0)(= len-nb 3))
     ;;  1 0 3  ->  do nothing ok , or select other two also
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;;
     (debug #t "EDGE MCB103 : hello .~%")
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
	 (error "absurdity"))))))



(defmacro edge-203-macro ()
  `(when 
       (and (= len-m 2)(= len-c 0)(= len-nb 3))
     ;;
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;;
     (debug #t "edge case MCB203: debug help len-m-c-nb ~a ~a ~a~%" len-m len-c len-nb)
     ;; 2 neighbours must . no neighbours in cannot , 3 neighbours in total
     ;; must choose only remaining
     (let* ((without-cant (filter (lambda (cell) (not (member cell cannot))) nb))
	    (choices (filter (lambda (cell) (not (member cell must))) without-cant)))
       (debug #t "choices =~a~%" choices)
       (let ((p1 (first choices)))
	 ;; pick p1 
	 (search3 (cons p1 must) cannot (cdr next))))))



(defmacro edge-113-macro ()
  `(when 
       (and (= len-m 1)(= len-c 1)(= len-nb 3))
     ;; 1 neighbour already must selected , 1 cannot be selected ,
     
     (when (eq? handled #t) (format #t "possible duplicate impl EDGES~%") (error "foo"))
     ;; set handled flag
     (set! handled #t)
     ;;
     ;; nothing to do but continue without selection because if select last
     ;; neighbour as must , have 2 neigbours selected violates constraint only 1 or 3
     ;; neighbours selected as must.
     (search3 must cannot (cdr next))))


;; =========== end search macros ==================================

;; sorter for coordinates
;; p1 (x1 y1)
;; p2 (x2 y2)



(define coord-compare
  (lambda (p1 p2)
    (let ((x1 (first p1))
	  (y1 (second p1))
	  (x2 (first p2))
	  (y2 (second p2)))
      (let ((result #t))	
	(cond
	 ((< x1 x2) (set! result #t))
	 ((> x1 x2) (set! result #f)) ;;
	 ((< y1 y2) (set! result #t))
	 (#t (set! result #f)))	
	(format #t "point1: ~a " p1)
	(format #t "x1: ~a " x1)
	(format #t "y1: ~a ~%" y1)
	(format #t "point2: ~a " p2)
	(format #t "x2: ~a " x2)
	(format #t "y2: ~a ~%" y2)
	(format #t "comparing ~a is less than ~a ? => ~a ~%" p1 p2 result)
	result))))



;; ========= TEST SUITES need SRFI - 64 =============
(test-begin "sort-test")
;; to be sorted
;; hopefully this is a local definition inside sort-test
(define input '((1 1)(2 2)(1 2)(2 1)(3 1)(3 2)(3 3)(1 3)(2 3)(1 2)))
;; REquuire expression eval to true but its false
;;(test-assert #f)
;; Require that an expression evaluate to true.
;; (test-assert (vector? v))
;; Test that an expression is eqv? to some other expression.
;; (test-eqv 99 (vector-ref v 2))
;; (vector-set! v 2 7)
;; (test-eqv 7 (vector-ref v 2))
(define output-desired '((1 1)(1 2)(1 2)(1 3)(2 1)(2 2)(2 3)(3 1)(3 2)(3 3)))

(test-equal output-desired (sort input coord-compare))
;; Finish the testsuite, and report results.
(test-end "sort-test")

;; ==========================================================


(define solutions-count 0)
(define solutions (make-hash-table))
(define shortest-solutions '())
(define shortest-solution-length #f)


(define search3
    (lambda (must cannot next)
      (cond
       ((null? next)

	(let ((sorted-must
	       (sort must (lambda (m1 m2) ;; destructure
			    (let ((m1x (first m1))
				  (m1y (second m1))
				  (m2x (first m2))
				  (m2y (second m2)))
			      (if (< m1x m2x)
				  #t
				  (if (< m1y m2y)
				      #t
				      #f)))))))

	  ;; is this a unique solution
	  (let ((found (hash-ref solutions sorted-must #f)))
	    (cond
	     (found #f) ;;already in solutions hash table
	     (#t ;; new solution
	      ;; THIS SHOULD BE A SOLUTION AS ALL SATISFIED
	      ;;(debug #t "FINAL NO MORE : ~a~%~%" must)
	      ;; store #t flag at whole solution - hopefully hash uses equal? 
	      (hash-set! solutions sorted-must #t)
	      (set! solutions-count (+ 1 solutions-count))
	      ;; (format #t "~a" sorted-must)
	      ;; (format #t "~%")

	      ;; hidden solutions count for now 
	      ;;(format #t "\"solution ~a \"~%" solutions-count)

	      ;; determine if shortest solution so far
	      ;; ensure shortest-solution-length has integer value 
	      (when (eq? shortest-solution-length #f)
		(set! shortest-solution-length (length sorted-must)))

	      ;;
	      (when (= (length sorted-must) shortest-solution-length)
		(set! shortest-solutions (cons sorted-must shortest-solutions)))

	      ;; record new best shortest solution length
	      (when (< (length sorted-must) shortest-solution-length)
		(set! shortest-solutions (list sorted-must))
		(set! shortest-solution-length (length sorted-must))
		(format #t "new best shortest solution length ~a ~%"
			shortest-solution-length)
	      ;; report answer in terms of smalltalk language
		(must->smalltalk sorted-must))
 	      
	      ;; (format #t "~%")
	      ;;(error "~%;;------------------NO MORE NEXT VALUES-----------------~%")
	      ;; must))
	      ))))) ;; null? next - ie all squares have been processed / reasoned about
	      
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
		 (debug #t "~%~%considering ~a~%" opt)
		 (debug #t "m(~a) ~a : must(~a) ~a~%" len-m m (length must) must)
		 (debug #t "c(~a) ~a : cannot ~a ~%" len-c c cannot)
		 (debug #t "neigh(~a) ~a ~%" len-nb nb)
		 
		 (cond
		  ((corner? opt)
		   ;; ================ CORNER CASES ======================
		   (debug #t "corner square !~%")
		   (debug #t "corner case : prem-debug help len-m-c-nb ~a ~a ~a~%"
			  len-m len-c len-nb)		     

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
		   (let ((handled #f))

		     (corner-002-macro)
		     (corner-012-macro)
		     (corner-112-macro)
		     (corner-202-macro)
		     (corner-102-macro)		     
		     (corner-022-macro) 
		     
		     
		     ;; ((and (= len-m 0) (= len-c 0) (= len-nb 2))
		     ;;  (debug #t "corner case 1 - free pick one neighbour, reject other ~%")
		     ;;  (let ((p1 (car nb))
		     ;; 	 (p2 (car (cdr nb))))
		     ;;    ;; pick first
		     ;;    (search3 (cons p1 must) (cons p2 cannot) (cdr next))
		     ;;    ;; pick second
		     ;;    (search3 (cons p2 must) (cons p1 cannot) (cdr next)))
		     ;;  ) ;; m0 c0 nb2
		     
		     (when (not handled)
		       (debug #t "corner case : debug help len-m-c-nb ~a ~a ~a~%"
			      len-m len-c len-nb)		     
		       (error "corner case not handled")))
		   ;; assert length nb == 2
		   ) ;; end corner
		  ((edge? opt)
		   ;; ================ EDGE  CASES ======================
		   (debug #t "edge square ")
		   ;; cases 
		   ;; must cannot neighbour
		   (let ((handled #f))
		     
		     (edge-003-macro)
		     (edge-013-macro)
		     (edge-023-macro)		  		  
                     (edge-103-macro)
		     (edge-113-macro)
		     (edge-203-macro)
		     (edge-303-macro)		     
		     (edge-213-macro)		     
		     (edge-123-macro)
		     (edge-033-macro)
		     

		     
		     (when (not handled)
		       (debug #t "edge case : debug help len-m-c-nb ~a ~a ~a~%"
			      len-m len-c len-nb)
		       (error "edge case not handled!"))) ;;let hanlded
		   );; end edge
		  ((inside? opt)
		   ;; ================ INSIDE CASES ======================
		   (debug #t "inside square ")

		   (let ((handled #f))
		     
		     (inside-214-macro)		  
		     (inside-124-macro)
		     (inside-204-macro)
		     (inside-304-macro)
		     (inside-104-macro)
		     (inside-114-macro)
		     (inside-024-macro)
		     (inside-034-macro)
		     (inside-014-macro) ;; TODO
		     
		     
		     ;; assert length nb == 4 
		     ;; 4 in must-nb -> contradiction , do not continue!
		     ;; 3 in must-nb -> fully defined , no action , move next square
		     ;; caveat
		     ;; 2 in must-nb 
		     ;; 1 in must-nb 
		     ;; 0 in must-nb 0 1 2 3 4 in cannot-nb

		     (when (not handled)
		       (debug #t "inside case : debug help len-m-c-nb ~a ~a ~a~%"
			      len-m len-c len-nb)
		       (error "inside case not handled!"))
		     
		     ) ;; handled flag cond
		   );; inside? opt
		  )))))))) ;; search3
















;; (cond
;; 	((and (null? must-nb)(null? cannot-mb))
;; 	 (let ((take1nb (pick1 nb))
;; 	       (take3nb (pick3 nb)))
;; 	   (let ((len1 (length take1nb))
;; 		 (len3 (length take3nb)))
;; 	   (cond
;; 	    ((= len1 1) (error "not implemented yet"))
;; 	    ((= len3 3) (error "not implemented yet"))))))))))))))




;; just look at the beauty of this triangle
(define run
  (lambda ()
    (set! solutions-count 0)
    (set! solutions (make-hash-table))
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
  		  (10 2)(9 3)(8 4)(7 5)(6 6)(5 7)(4 8)(3 9)(2 10)
                  (10 3)(9 4)(8 5)(7 6)(6 7)(5 8)(4 9)(3 10)
		  (10 4)(9 5)(8 6)(7 7)(6 8)(5 9)(4 10)
		  (10 5)(9 6)(8 7)(7 8)(6 9)(5 10)
		  (10 6)(9 7)(8 8)(7 9)(6 10)
		  (10 7)(9 8)(8 9)(7 10)
		  (10 8)(9 9)(8 10)
		  (10 9)(9 10)
		  (10 10)
		  )))
      (search3 must cannot next))))






;; helper turn list into smalltalk message
(define must->smalltalk
  (lambda (xs)
    (format #t "{")
    ;; cannot map over - need to know if this is last element of sequence
    (let ((lim (- (length xs) 1)))
      (let loop ((i 0))
	(let ((pr (list-ref xs i)))
	  (cond
	   ((>= i lim) ;; last item in list
	    (format #t " ~a~a~a " (first pr) "@" (second pr)))
	   (#t ;; not last item include dot 
	    (format #t " ~a~a~a . " (first pr) "@" (second pr))
	    (loop (+ i 1))))))
      (format #t "} . "))))

(must->smalltalk '((3 3) (4 2) (1 4) (2 3) (3 2) (2 2) (1 2)))










