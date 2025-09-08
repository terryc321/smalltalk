
#|

make a text file with contents - call file <something>.txt 
we want the txt file extension -- FIXME why

_ means empty square
x means wall
a means the player 
b means a box can move

text file fairly loose in acceptance
just need same number visible characters on each line
empty lines are ignore
spaces tabs are ignored


a_b_
____

same game as 

a _ b _
_ _ _ _ 

(level-reader "<your-text-file>.txt")






read level 2 auto generate width height 
fill with $_
pick out a x b c characters
place player at correct location
output as smalltalk code 
|#

(ql:quickload :uiop)

(defpackage :foo
  (:use :cl))
(in-package :foo)


;; remove spaces from string
(defun remove-spaces-from-string (s)
  (remove #\tab (remove #\space s)))





#|
(defparameter *board* 
   (uiop:read-file-lines "level1.txt"))

(defparameter *board2* (remove-if (lambda (s) (= (length s) 0))
				  *board*))


(defparameter *original-string* "a a a a a a a ")
(defparameter *cleaned-string* (remove-spaces-from-string *original-string*))

(defparameter *board3* (mapcar #'remove-spaces-from-string
			       *board2*))

(defparameter *width* (length (car *board3*)))
(defparameter *height* (length *board3*))

(mapcar (lambda (s) (assert (= (length s) *width*))) *board3*)
|#
;; ===== ========= 

(defun level-reader(filename)
  ;; strip postfix .txt from filename 
  (let ((func-name (subseq filename 0 (- (length filename) 4))))
  (let* ((board (mapcar #'remove-spaces-from-string 
			(remove-if (lambda (s) (= (length s) 0)) 
				   (uiop:read-file-lines filename))))
	 (width (length (car board)))
	 (height (length board)))
    ;; check all lines are same length
    (mapcar (lambda (s) (assert (= (length s) width))) board)
    ;;
    ;; show text preview
    (format t "~%#(")
    (loop for str in board do
      (format t "~%")
      (let ((strlist (coerce str 'list)))
	(loop for ch in strlist do 
	  (format t "~a " ch))))
    (format t "~%).~%")
    ;; 
    ;; generate actual smalltalk code
    ;;
    (format t "Sokoban compile: '~a~%" func-name)
    (format t "width := ~a . ~%" width)
    (format t "height := ~a . ~%" height)
    (format t "board := Dictionary new . ~%")
    (format t "  1 to: width do: [:x |   	1 to: height do: [:y |  board at: (x@y) put: $_ ]] .~%")
    (let ((x 0)(y 0))
      (loop for str in board do
	(incf y)
	(setq x 1)
	(let ((strlist (coerce str 'list)))
	  (loop for ch in strlist do 
	    (when (eq ch #\a) 
	      (format t "player := (~a@~a) .~%" x y) )
	    (format t "board at:(~a@~a) put: $~a .~%" x y ch) 
	    (incf x)))))
    (format t " self changed . ' classified: 'levels' . ~%"))))







;;(level-reader "level32.txt")


 


