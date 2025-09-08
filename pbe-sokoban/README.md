
# sokoban

## final.st 

this file contains a simple model that can represent any sokoban board 

Sokoban class

## soko.st

this file contains a front end gui in smalltalk 

in order to do this we need to give view access to sokoban 

SokobanView class 

## 

pharo 12 - SOKOBAN image continue ...

```

width - width of board
height - height of board
board  - dictionary given a point (x@y)

player represented by $a character 
walls represented by $x character
boxes can move represented by $b character

when we move player we move either player alone or move a box and a player 
we need to track where the player is using a (x@y) 
also we need to update the board dictionary so it stays consistent with where the player 
actually is

```

anything below this line is ignored 

==================================================================================


need to figure out a 2d representation 

what if player wants to push a box off the board ?

how can tell if the game has been won ?

if can write a parser for smalltalk system we could analyse it using common lisp

or somehow hook common lisp into the runtime somehow

2 d array problem - variations in cuis squeak and pharo dropped array2d , matrix 

use a point (x@y) to represent point on the board 

Look at a board that is bounded so we cannot escape

```
  1 2 3 4 5 
x x x x x x 
x a _ x c x 1
x _ b _ _ x 2
x _ _ _ _ x 3
x x x x x x 
```

If we then focus on the playable area

```
constraints
1 <= x <= 4 
1 <= y <= 3
a is the player can move up down left right 
a starts at (1,1)
b is moveable box it can be pushed by a 
_ are empty squares
c is also an empty square where box b should be placed 
c is target location of box b
x is a barrier through which neither a or b can pass

aim is to move player a in such a fashion to push box b into square c


  1 2 3 4 
  a _ x c  1
  _ b _ _  2
  _ _ _ _  3
  
```



```
BorderedMorph subclass: #Sokoban
       instanceVariableNames: 'width height board'
       classVariableNames: ''
       package: 'PBE-Sokoban'.


Sokoban compile: 'initialize
    super initialize.

	width := 4 .
	height := 3 .

	board := Dictionary new . 
	1 to: width do: [:x | 
 		1 to: height do: [:y | 
			board at: (x@y) put: $_ ]] .
  
	board at:(1@1) put: $b . 
	board at:(3@1) put: $x .
	player := (1@1).'	
  classified: 'actions'.


"can i go left "
lefta := [ |x y e| 
	x := player x . 
	y := player y . 
	e := board at: ((x - 1) @ y) ifAbsent: nil.
	x >= 2 ifTrue: [ e = $_ ifTrue: [ ^ yes ]]] value . 


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


