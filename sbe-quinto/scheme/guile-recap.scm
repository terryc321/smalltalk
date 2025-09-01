

#|

specifics related to guile scheme

|#

"x gets overwritten , so save it to tmp first "
(defmacro swap! (x y)
  (let ((tmp x))
    (set! x y)
    (set! y tmp)))



