
THE CODE IN THIS FILE IS BAD 

SEE final.st INSTEAD for sokoban model 

MISTAKES IN CAN GO UP / DOWN / LEFT / RIGHT





"== evaluate the Sokoban class definition first  == works pharo12 , not pharo 13 or 14"
BorderedMorph subclass: #Sokoban
       instanceVariableNames: 'width height board player'
       classVariableNames: ''
       package: 'PBE-Sokoban'.

"=== after , we can do it on the later routines  "

Sokoban compile: 'initialize
    super initialize.
    self reset.'	
  classified: 'startup'.


Sokoban compile: 'reset
    width := 4 .
    height := 3 .
    board := Dictionary new . 
    1 to: width do: [:x | 
    	1 to: height do: [:y | 
	  board at: (x@y) put: $_ ]] .
  
    board at:(1@1) put: $a . 
    board at:(2@2) put: $b . 
    board at:(3@1) put: $x .
    player := (1@1).'	
  classified: 'reset'.


#(
  1 2 3 4
1 a _ x c
2 _ b _ _	  
3 _ _ _ _
).


"create an instance soko to investigate methods "
Transcript clear.
soko := Sokoban new . 
soko reset . 
soko showBoard .


"can i go left ,  "
" _ a   : if decide move left need to put player at x-1 move a from where it is , to where needs to be"
" _ b a  : if decide move left push box left also player at x-1 move a from where it is to , put b at x-2"
Sokoban compile: 'canGoLeft
| x y e f |
x := player x .
y := player y .
e := board at: ((x - 1) @ y) ifAbsent: nil.
f := board at: ((x - 2) @ y) ifAbsent: nil.
x >= 2 ifTrue: [ e = $_ ifTrue: [ ^ [ 
          board at: ((x - 1) @ y) put: $a .
          board at: (x @ y) put: $_ . 
          player := (x - 1 @ y).
]]].
x >= 3 ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ [ 
          board at: ((x - 2) @ y) put: $b .
	  board at: ((x - 1) @ y) put: $a .
	  board at: (x @ y) put: $_ .
          player := (x - 1 @ y).
]]]].
^ false'
 classified: 'ask'.

" ==========  =================== ============= ================= ===============  "

"can i go right - depends on size board and whats there "
Sokoban compile: 'canGoRight
| x y e f |
x := player x .
y := player y .
e := board at: ((x + 1) @ y) ifAbsent: nil.
f := board at: ((x + 2) @ y) ifAbsent: nil.
x <= (width - 1) ifTrue: [ e = $_ ifTrue: [ ^ [ 
          board at: ((x + 1) @ y) put: $a .
          board at: ( x @ y) put: $_ . 
          player := (x + 1 @ y).

]]].
x <= (width - 2) ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ [ 
          board at: ((x + 2) @ y) put: $b .
	  board at: ((x + 1) @ y) put: $a .
	  board at: (x @ y) put: $_ .
          player := (x + 1 @ y).
]]]].
^ false'
 classified: 'ask'.

" ==========  =================== ============= ================= ===============  "

Sokoban compile: 'canGoDown
| x y e f |
x := player x .
y := player y .
e := board at: (x @ (y + 1)) ifAbsent: nil.
f := board at: (x @ (y + 2)) ifAbsent: nil.
y <= (height - 1) ifTrue: [ e = $_ ifTrue: [ ^ [ 
          board at: (x @ (y + 1)) put: $a .          
          player := (x @ (y + 1)).
	  board at: (x @ y) put: $_ .   
]]].
y <= (height - 2) ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ [ 
          board at: (x @ (y + 2)) put: $b .
	  board at: (x @ (y + 1)) put: $a .
          player := (x @ (y + 1)).
	  board at: (x @ y) put: $_ .
]]]].
^ false'
 classified: 'ask'.

" ==========  =================== ============= ================= ===============  "

Sokoban compile: 'canGoUp
| x y e f |
x := player x .
y := player y .
e := board at: (x @ (y - 1)) ifAbsent: nil.
f := board at: (x @ (y - 2)) ifAbsent: nil.
y >= 2 ifTrue: [ e = $_ ifTrue: [ ^ [ 
          board at: (x @ (y - 1)) put: $a .
          player := (x @ (y - 1)).
          board at: (x @ y) put: $_ . 
]]].
y >= 3 ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ [ 
          board at: (x @ (y - 2)) put: $b .
	  board at: (x @ (y - 1)) put: $a .
          player := (x @ (y - 1)).
	  board at: (x @ y) put: $_ .
]]]].
^ false'
 classified: 'ask'.



Sokoban compile: 'showBoard
 | val | 
  1 to: height do: [:y | 
    Transcript cr .
    1 to: width do: [:x | 
	val :=  board at: (x@y) .
       Transcript show: val ; show: '' '' ]] .
    Transcript cr . '
 classified: 'show'.




```

when can player a go left ?

```
  1 2 3
1 _ a
  
1 _ b a 
  
```

canGoLeft 

player x >= 2 and thing at player x - 1 is blank

lefta := [ |x y e| 
	x := player x . 
	y := player y . 
	e := board at: ((x - 1) @ y) ifAbsent: nil.
	x >= 2 ifTrue: [ e = $_ ifTrue: [ ^ true ]]] value . 

player x >= 3 and thing at player x - 1 is moveable box and thing at player x - 1 is empty

leftb := [ |x y e f| 
	x := player x . 
	y := player y . 
	e := board at: ((x - 1) @ y) ifAbsent: nil.
	f := board at: ((x - 2) @ y) ifAbsent: nil.	
	x >= 3 ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ true] ]]] value . 

otherwise no
    ^ false


canGoLeft 

La := [ |x y e| 
	x := player x . 
	y := player y . 
	e := board at: ((x - 1) @ y).
	x >= 2 ifTrue: [ e = $_ ifTrue: [ ^ yes ]]] value . 


Lb 



true and false. 


board at:(3@2) put: $x .


```
manual.st - contains basic morphic user interface keyboard interaction for simple view
```


```
sokoban using object oriented programming

some sort of 2d grid 
player moves up left right down

walls
boxes to be moved
target squares X X X
where boxes need to cover

xxxxxxxxxxxxxx
xox    xxx   x
x xxx  xx  xxx
x x    xxx   x
x       x    x
xxxxxxxxxxxxxx

lets for momemnt assume we have a grid that can be solved

xxxxxxxxxxxxxxxxx
x               x 
x               x 
x    Y          x 
x          B    x 
x               x 
x               x 
x               x 
x              Tx 
xxxxxxxxxxxxxxxxx

what movements are required to move box B to target T



```


