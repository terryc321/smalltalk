

(defmacro macro-1 (x)
  `((and (= ,x 0)) (format t "x = 0 ~%")))

(defmacro macro-2 (x)
  `((and (= ,x 1)) (format t "x = 1 ~%")))

(defmacro macro-3 (x)
  `((and (= ,x 2)) (format t "x = 2 ~%")))




;;struggling to understand why cannot put a macro inside a normal cond statement

(eval `(defun test ()
	 (loop for x from 0 to 4 do    
	       ,(list 'cond
		  (macro-1 'x)
		  (macro-2 'x)
		  (macro-3 'x)))))


(test)

(macro-1)

'`(,@(macro-1) ,@(macro-2))


(defmacro swap (a b)
  (let ((tmp (gensym "tmp")))
    `(progn
       (setq ,tmp ,a)
       (setq ,a ,b)
       (setq ,b ,tmp))))


(macroexpand-all (let ((a 1)(b 2))
		   (swap a b)
		   (list a b)))

;; C-c M-m   macroexpand-all   slime+emacs
(let ((a 1)(b 2))
  (swap a b)
  (list a b))


  





;; (bill)
;; (fred)
;; (bob)

#|
(loop for x from 0 to 2 do 
      (cond
	(bill)
	(fred)
	(bob)))


(loop for x from 0 to 2 do 
      (cond
	((and (= x 0)) (format t "x = 0 ~%"))
	((and (= x 1)) (format t "x = 1 ~%"))
	((and (= x 2)) (format t "x = 2 ~%"))
	))
|#

;;(eval (list 'bill))

;; (cond
;;   (eval (list 'bill)))

;; (eval (let ((x 1))
;; 	 (if (bill)
;; 	     (if (fred)
;; 		 (if (bob)
;; 		     "who knows")))))




