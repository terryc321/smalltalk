
handleKeyDown: anEvent
    | keyValue x y x2 y2 wx wy wx2 wy2 |
   " wx wy window x y "
   " x y position of "
   "how do "
    "Transcript show: 'key stroke event '; cr ."
    keyValue := anEvent keyValue.
    "keyValue inspect."
    " f = 102 F=70 "
    " w = 119  W=87 "
    " a = 97  a=65 "
    " s = 115  S=83 "
    " d = 100  D=68 "
    wx := self position x .
    wy := self position y .
    x := player position x .
    y := player position y .
    x2 := player extent x .
    y2 := player extent y . 
    wx2 := self extent x .
    wy2 := self extent y . 
    keyValue = 30 ifTrue: [ Transcript show: 'you pressed an arrow key ' ; cr ] . 
    keyValue = 100 ifTrue:[ x := x + 10 ] . "right"
    keyValue = 119 ifTrue:[ y := y - 10  ] . "up"
    keyValue = 97 ifTrue:[ x := x - 10 ] . "left"
    keyValue = 115 ifTrue:[ y := y + 10 ] . "down"
    x < wx ifTrue: [ x := wx ] .
    y < wy ifTrue: [ y := wy ] .
    (x + x2 )> (wx + wx2 ) ifTrue: [ x := wx + wx2 - x2  ] .
    (y + y2 )> (wy + wy2 ) ifTrue: [ y := wy + wy2 - y2  ] .
    player position: x @ y . 

    
    "anEvent inspect."
    keyValue = Character arrowLeft 
        ifTrue: [self position: self position - (0 @ 10).
	   Transcript show: 'up arrow pressed' ; cr . 
	].
    keyValue = 31 "down arrow"
        ifTrue: [self position: self position + (0 @ 10).
		   Transcript show: 'down arrow pressed' ; cr . 
	].
    keyValue = 29 "right arrow"
        ifTrue: [self position: self position + (10 @ 0).
		   Transcript show: 'right arrow pressed' ; cr . 
	].
    keyValue = 28 "left arrow"
        ifTrue: [self position: self position - (10 @ 0).
		   Transcript show: 'left arrow pressed' ; cr . 
	]


		    
