
# sokoban

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
x x x x x x 1 
x a _ x c x 2
x _ b _ _ x 3
x _ _ _ _ x 4
x x x x x x 5
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

aim is to move player a in such a fashion to push box b into square c


  1 2 3 4 
  a _ x c  1
  _ b _ _  2
  _ _ _ _  3
  
```



```
width := 5 .
height := 5 .
board := Dictionary new . 
1 to: width do: [:x | 
 1 to: height do: [:y | 
  board at: (x@y) put: $_ ]] .
  
board at:(1@1) .

```
  







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


