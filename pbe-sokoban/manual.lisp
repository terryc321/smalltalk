

;; generate smalltalk code for handleKeystroke: anEvent
#|
Sokoban compile: 'handleKeyDown: anEvent
    | keyValue tr |
    tr := [ :message | Transcript show: message ; cr ] .
    keyValue := anEvent keyValue.
keyValue = 9 ifTrue: [ Transcript show: ''you pressed the tab   key '' ; cr ].
keyValue = 27 ifTrue: [ Transcript show: ''you pressed the escape key '' ; cr ].
...
..
... 
|#

;; alphabetical lower case letters
(loop for ch from 32 to 255 do
  (format t "keyValue = ~a ifTrue: [ Transcript show: ''you pressed the ~a key '' ; cr ].~%"
	  ch (code-char ch)))



