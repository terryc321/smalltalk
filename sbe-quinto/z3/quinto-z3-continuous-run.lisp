

#|

using save-lisp-and-die with uiop already installed

;; for uiop:run-program ;; shell out
;;(ql:quickload :uiop) 


purpose of continous run is to use pre-generated files

quinto-z3-header.z3
quinto-z3-solutions.z3
quinto-z3-footer.z3

CAT them all together to make

continuous.z3
z3 continous.z3 > continuous.z3out
scan continuous.z3out for SAT word followed by a SOLUTION
we decode solution
we look for any square that has TRUE boolean value
collect all squares with TRUE booleans and make a big assertion 

 (assert (not (and s_01_01 s_02_02 s_03_03 ..)))

this is then appended to quinto-z3-solutions.z3 

quinto-z3-solutions.z3 ends up looking like ...

 (assert (not (and s_01_01 s_02_02 s_03_03 ..)))
 (assert (not (and s_01_01 s_02_02 s_03_03 ..)))
 (assert (not (and s_01_01 s_02_02 s_03_03 ..)))
 (assert (not (and s_01_01 s_02_02 s_03_03 ..)))
 ...

we also append to smalltalk file which
we can fix up {  .} mistake using a shell command 
quinto-z3-solutions.st 



initial run is to generate a z3 file that once fed to z3 will generate a solution file

(run) -- deletes auto.z3 auto.z3out 


compute required board computations for z3 

corners done already

boolean variables from s_1_1 to s_10_10 , 100 of them

corner logic

side logic

|#

(ql:quickload :uiop)


(defpackage :tc
  (:use :cl))
(in-package :tc)



(defparameter *solutions* nil)

(defparameter symbol-hash (make-hash-table :test #'eq)) ;;qualp))

(defparameter solution-hash (make-hash-table :test #'equalp))

(defun off-board-p (square)
  (let ((x (car square))
	(y (car (cdr square))))
    (cond
      ((< x 1) t)
      ((< y 1) t)
      ((> x 10) t)
      ((> y 10) t)
      (t nil))))




(defun corner (square neighbours)
  "corner - two neighbours - only one which can be , other must be false"
  (assert (= (length neighbours) 2))
  (let* ((result nil)
	 (z3-symbols (mapcar #'make-z3-symbol neighbours))
	 (a (first z3-symbols))
	 (b (second z3-symbols)))
    (format t ";; corner ~a~%" (make-z3-symbol square))
    (setq result (format nil "(assert (xor ~a ~a))~%" a b))
    (format t "~a~%" result)))



(defun edge (square neighbours)
  "edge - three neighbours
   can be only one - with two others not
   or can be all three together
  "
  (assert (= (length neighbours) 3))
  (let* ((z3-symbols (mapcar #'make-z3-symbol neighbours))
	 (a (first z3-symbols))
	 (b (second z3-symbols))
	 (c (third z3-symbols)))    
    (format t ";; edge ~a~%" (make-z3-symbol square))
    (format t "(assert (xor (and ~a (not ~a) (not ~a))~%" a b c)
    (format t "             (and ~a (not ~a) (not ~a))~%" b a c)
    (format t "             (and ~a (not ~a) (not ~a))~%" c a b)
    (format t "             (and ~a ~a ~a)))~%" a b c)))



(defun internal (square neighbours)
  "internal - four neighbours
   can be only one - with three others not
   or can be three - with one not
  "
  (assert (= (length neighbours) 4))
  (let* ((z3-symbols (mapcar #'make-z3-symbol neighbours))
	 (a (first z3-symbols))
	 (b (second z3-symbols))
	 (c (third z3-symbols))
	 (d (fourth z3-symbols)))
    
    (format t ";; internal ~a~%" (make-z3-symbol square))
    ;; one square is MUST , other three is CANNOT
    (format t "(assert (xor (and ~a (not ~a) (not ~a) (not ~a))~%" a b c d)
    (format t "             (and ~a (not ~a) (not ~a) (not ~a))~%" b a c d)
    (format t "             (and ~a (not ~a) (not ~a) (not ~a))~%" c a b d)
    (format t "             (and ~a (not ~a) (not ~a) (not ~a))~%" d a b c)
    ;; three squares is MUST other one square is CANNOT
    (format t "             (and (not ~a) ~a ~a ~a)~%" a b c d)
    (format t "             (and (not ~a) ~a ~a ~a)~%" b a c d)
    (format t "             (and (not ~a) ~a ~a ~a)~%" c a b d)
    (format t "             (and (not ~a) ~a ~a ~a)))~%" d a b c)))



(defun on-board-p (square)
  (not (off-board-p square)))

(defun make-z3-symbol (square)
  (let ((x (car square))
	(y (car (cdr square))))
    (format nil "s_~a_~a" x y)))


(defun the-symbol-definitions ()
  (loop for x from 1 to 10 do
    (loop for y from 1 to 10 do
	(let ((square (list x y)))
	  (format t "(declare-const ~a bool)~%"(make-z3-symbol square))))))



(defun traverse ()
  (loop for x from 1 to 10 do
    (loop for y from 1 to 10 do
      (let ((neighbours (list (list (- x 1) y)
			      (list (+ x 1) y)
			      (list x (- y 1))
			      (list x (+ y 1)))))
	(let ((square (list x y))
	      (true-neighbours (remove-if-not #'on-board-p neighbours)))
	  (format t "~%;;neighbours of ~a => ~a ~%" (make-z3-symbol square) true-neighbours)

	  (let ((len (length true-neighbours)))
	    (cond
	      ((= len 2) (tc::corner square true-neighbours))
	      ((= len 3) (tc::edge square true-neighbours))
	      ((= len 4) (tc::internal square true-neighbours))
	      (t (error "cannot exist")))))
	t))))


	    




(defun conclusion ()
  (format t "(check-sat)~%")
  (format t "(get-model)~%"))

(defun fill-symbol-hash ()
  (loop for x from 1 to 10 do
    (loop for y from 1 to 10 do
      (let ((key (intern (format nil "S_~a_~a" x y)))
	    (value (list x y)))
	(setf (gethash key symbol-hash) value)))))


(defun reset-solution-hash ()
  (loop for x from 1 to 10 do
    (loop for y from 1 to 10 do
      (let ((key (list x y))
	    (value 'dummy))
	(setf (gethash key solution-hash) value)))))


;; seq (DEFINE-FUN S_9_8 NIL BOOL TRUE)
(defun decode (seq)
  (let ((s (second seq))
	(value (fifth seq)))
    (let ((square (format nil "~a" (gethash s symbol-hash))))
      ;;(format t "SEQ = ~a : (~a) : (~a) ~%" seq (second seq) (fifth seq))
      (cond
	((equalp value 'true)
	 (format t "~a <- ~a ~%" square 'true)
	 (setf (gethash square solution-hash ) 'must))
	
	((equalp value 'false)
	 ;;(format t "~a <- ~a ~%" square 'false)
	 (setf (gethash square solution-hash ) 'cant))
	 
	(t (error "expected true or false"))))))


;; go to stdout
(defun show-solution-grid ()
  (format t "~%")
  (loop for y from 1 to 10 do
    (loop for x from 1 to 10 do
      (let* ((square (list x y))
	     (str-square (format nil "~a" square)))
	;;(format t "looking at square (~a)~%" str-square)
	(let ((val (gethash str-square solution-hash)))
	  (cond
	    ((eq val 'must) (format t "x"))
	    ((eq val 'cant) (format t "o"))
	    (t
	     (format t "dirty solution hash on ~a ~%" val)
	     (error "dirty solution hash values"))))))
    (format t "~%")))


;; go to 
(defun show-solution-as-smalltalk ()
  (with-open-file (*standard-output* "quinto-z3-solutions.st"
                                   :direction :output
                                   :if-exists :append)  
  (format t "{")
  (loop for y from 1 to 10 do
    (loop for x from 1 to 10 do
      (let* ((square (list x y))
	     (str-square (format nil "~a" square)))
	;;(format t "looking at square (~a)~%" str-square)
	(let ((val (gethash str-square solution-hash)))
	  (cond
	    ((eq val 'must)
	     (format t "~a@~a . " x y)))))))
  (format t "} . ")))



(defun show-solution-as-negated-z3 ()
  (with-open-file (*standard-output* "quinto-z3-solutions.z3"
                                   :direction :output
                                   :if-exists :append)  
  (format t "~%")
  (format t "(assert (not (and ")
  (loop for y from 1 to 10 do
    (loop for x from 1 to 10 do
      (let* ((square (list x y))
	     (str-square (format nil "~a" square)))
	;;(format t "looking at square (~a)~%" str-square)
	(let ((val (gethash str-square solution-hash)))
	  (cond
	    ((eq val 'must)
	     (format t "s_~a_~a " x y)))))))
  (format t ")))~%")))


;; can redirect stdout to file - then 
(defun run ()
  ;;
  (fill-symbol-hash)
  ;;
  (reset-solution-hash)

  ;;(uiop:run-program "cat quinto-z3-header.z3 > auto.z3")

  ;; generate auto.z3 -- can we do this from memory?
  ;; (with-open-file (*standard-output* "auto.z3"
  ;;                                  :direction :output
  ;;                                  :if-exists :supersede)
  ;;   (the-symbol-definitions)
  ;;   (traverse)
  ;;   (conclusion))
  
  ;; run shell script - externally run z3 -
  ;; (uiop:run-program `("z3" "auto.z3") :output "auto.z3out")
  ;; read z3 output generated

  ;; open solution file 
  
  (with-open-file (stream "auto.z3out")
    (let ((outcome (read stream)))
      (when (equalp outcome 'sat)
	(let ((solution (read stream)))
	  ;;(format t "outcome was ~a~%" outcome)
	  ;;(format t "solution was ~a~%" solution)
	  (let ((decoded (mapcar #'decode solution)))
	      (show-solution-grid)
	      (show-solution-as-smalltalk)
	      (show-solution-as-negated-z3)))))))


#|

cat quinto-z3-header.z3 > 

|#

(defun continuous ()

  ;; delete any known solutions
  ;; rm -f auto.z3
  (uiop:run-program "rm -f auto.z3")
  (uiop:run-program "rm -f quinto-z3-solutions.z3")
  (uiop:run-program "rm -f quinto-z3-solutions.st")
  (uiop:run-program "touch quinto-z3-solutions.z3")
  (uiop:run-program "touch quinto-z3-solutions.st")
  
  (loop while t do 
  ;; generally wipes auto.z3out
  ;; build auto.z3 up again
  (with-open-file (stream "auto.z3" :direction :output :if-exists :supersede)
    (uiop:run-program "cat quinto-z3-header.z3" :output stream))
  
  (with-open-file (stream "auto.z3" :direction :output :if-exists :append)
    (uiop:run-program "cat quinto-z3-solutions.z3" :output stream)
    (uiop:run-program "cat quinto-z3-footer.z3" :output stream))

  ;; run z3 auto.z3 -> autoz3.out
  (with-open-file (stream "auto.z3out" :direction :output :if-exists :supersede)
    (uiop:run-program "z3 auto.z3" :output stream))
  ;; 
  (run) ;; common lisp code

  ;; rinse repeat
	))



