
just remember to consistently repeatedly fileOut before running long running computations.


(1 > 2) or: [(2 > 3) ].  false 
(2 > 1) or: [(2 > 3) ].  true 


this is now in cuis/ didnt work / back to squeak smalltalk 

Feature Require: 'morphs' . 

a := DraggableButtonMorph new openInHand.
b :=PluggableButtonMorph new openInHand.
c := HoverableButtonMorph new openInHand.
ButtonMorph 

a := BorderedBoxMorph new openInWorld .
a morphExtent: `50@50`.
a morphPosition: `100@100`.


b morphExtent: `50@50`.
b morphPosition: `100@100`.



SBE-Quinto game

* SBE for smalltalk by example

* Quinto game where click button and make neighbour buttons toggle state
  if they are on - they become off
  if they are off - they become on

* Quinto neighbour defined as within 1 square distance horizontal or vertical
  but not diagonal

 so if button at (x=5,y=6) is clicked then neighbours at following 
  (x=6,y=6)
  (x=4,y=6)
  (x=5,y=5)
  (x=5,y=7) toggle state

for an arbitrary A , B both positive integers 1, 2 , 3 ... 
(x=A,y=B) is clicked then neighbours at

(x=A-1,y=B)
(x=A+1,y=B)
(x=A,y=B-1)
(x=A,y=B+1)

are all toggled state

* state toggling

each button at arbitrary location (A,B) can either be ON or OFF

* how would we ever discover SimpleSwitchMorph




SimpleSwitchMorph >> SBECell

* sbecell is a simple switch morph

BorderedMorph >> SBEGame

* sbegame is a bordered morph 





c := SBECell new.
c explore.

s := SBEGame new openInWorld.
s reset.
s position: 600@300.
s moves.  
s toggleNeighboursOfCellAt: 1 at: 2 . 
s toggleNeighboursOfCellAt: 2 at: 2 .
"s toggleNeighboursOfCellAt: 10 at: 1 ."
s moves.  an OrderedCollection(1@2 2@2) 
s reset.
"bring morph to front screen ? "
s brute. 


"proposed solutions "
s setMoves: { 10@10 . 9@10 . 8@10 . 9@9 . 10@8 . 7@10 . 8@9 . 9@8 . 6@10 . 7@9 . 9@7 . 10@6 . 5@10 . 6@9 . 8@7 . 9@6 . 4@10 . 5@9 . 9@5 . 10@4 . 3@10 . 4@9 . 8@5 . 9@4 . 2@10 . 3@9 . 6@6 . 9@3 . 10@2 . 1@10 . 2@9 . 5@6 . 8@3 . 9@2 . 2@8 . 3@7 . 7@3 . 8@2 . 1@8 . 2@7 . 6@3 . 7@2 . 2@6 . 3@5 . 5@3 . 6@2 . 1@6 . 2@5 . 4@3 . 5@2 . 2@4 . 3@3 . 4@2 . 1@4 . 2@3 . 3@2 . 2@2 . 1@2 }.


s setMovesSlow: { 10@10 . 9@10 . 8@10 . 9@9 . 10@8 . 7@10 . 8@9 . 9@8 . 6@10 . 7@9 . 9@7 . 10@6 . 5@10 . 6@9 . 8@7 . 9@6 . 4@10 . 5@9 . 9@5 . 10@4 . 3@10 . 4@9 . 8@5 . 9@4 . 2@10 . 3@9 . 6@6 . 9@3 . 10@2 . 1@10 . 2@9 . 5@6 . 8@3 . 9@2 . 2@8 . 3@7 . 7@3 . 8@2 . 1@8 . 2@7 . 6@3 . 7@2 . 2@6 . 3@5 . 5@3 . 6@2 . 1@6 . 2@5 . 4@3 . 5@2 . 2@4 . 3@3 . 4@2 . 1@4 . 2@3 . 3@2 . 2@2 . 1@2 }.

s setMovesSpeed:  0.1 moves: { 10@10 . 9@10 . 8@10 . 9@9 . 10@8 . 7@10 . 8@9 . 9@8 . 6@10 . 7@9 . 9@7 . 10@6 . 5@10 . 6@9 . 8@7 . 9@6 . 4@10 . 5@9 . 9@5 . 10@4 . 3@10 . 4@9 . 8@5 . 9@4 . 2@10 . 3@9 . 6@6 . 9@3 . 10@2 . 1@10 . 2@9 . 5@6 . 8@3 . 9@2 . 2@8 . 3@7 . 7@3 . 8@2 . 1@8 . 2@7 . 6@3 . 7@2 . 2@6 . 3@5 . 5@3 . 6@2 . 1@6 . 2@5 . 4@3 . 5@2 . 2@4 . 3@3 . 4@2 . 1@4 . 2@3 . 3@2 . 2@2 . 1@2 }.





s setMoves: #( (3@3) (4@2) (1@4)  (2@3) (3@2)  (2@2)  (1@2) ).

(3@3) y  

"only 3 left"
s moves.  an OrderedCollection(1@1 2@1 4@2 6@1 7@1 10@1 10@2 8@3 4@3 8@4 2@4 1@4 6@5 5@5 3@6 3@7 1@8 1@7 2@8 2@9 1@9 1@10 2@9 2@10 4@9 5@9 6@9 6@9 7@9 7@9 7@10 8@10 10@10 10@10 9@9 9@9 10@9 10@8 10@5 9@6 9@6 8@7 9@7 10@8 7@7 9@8 10@7 9@8 10@7 9@7 10@8 10@6 2@9 3@10 3@9 2@10 2@9 1@10 2@10 3@9 4@9 5@8) 


"drawbacks using of gui is that"
s moves.  an OrderedCollection(10@10 1@10 1@10 1@10 1@10 1@10 1@10 1@10 1@10 1@10 1@10) 
(s at: 1 at: 10) isOn .  true 
#((1@10) (1@10)  (1@10) (1@10) (1@10) (1@10) (1@10)  (1@10) (1@10) (1@10)) size.    10 
"so if starts off as OFF , even number hits make it still OFF , how can it be ON ? "

"it can get into an inconsistent state "
"if click on border - does it register in two squares at once ?"

s moves.
s reset . 

| y |
Transcript clear.
y := 0 .
"using a block"
[y < 1000] whileTrue: [ Transcript show: y . y:= y + 1 ] .



s reset.
s delete.
s toggleNeighboursOfCellAt: 1 at: 1 . 
s toggleNeighboursOfCellAt: 2 at: 1 .
s toggleNeighboursOfCellAt: 10 at: 1 .

2  32 

Transcript show: 'cleared bottom row' ; cr . 
Transcript show: 'cleared bottom 3 rows !' ; cr . 
Transcript show: 'cleared bottom 5 rows !' ; cr . 
Transcript show: 'cleared bottom 8 rows !' ; cr . 

=========================================
hit 9@9
hit 9@9
hit 9@10
hit 8@10
hit 5@10
hit 4@10
hit 1@10
cleared bottom row
hit 10@8
hit 7@8
hit 6@8
hit 3@8
hit 2@8
hit 10@7
cleared bottom 3 rows !
hit 8@6
hit 5@6
hit 8@5
hit 5@5
hit 4@6
hit 4@5
hit 4@5
hit 5@5
hit 2@6
hit 2@6
hit 1@6
hit 1@5
hit 2@5
hit 2@4
hit 2@4
hit 1@4
cleared bottom 5 rows !
hit 10@4
hit 10@3
hit 6@4
hit 5@4
hit 3@3
hit 2@3
hit 2@2
hit 1@2
hit 8@2
hit 7@2

====================================================



we can programmatically toggle neighbours of a cell , as if that cell had been clicked

can we tell status of a cell at 1 1  ?
what are the status of a SimpleSwitchMorph ?

self turnOff
self turnOn

self isOn
self isOff

cells is a matrix 1,1 to 10,10 for cellsPerSide := 10

added at: x at: y method to SBEGame , returns cells at: x at: y which is a SimpleSwitchMorph
can then query if it is on or off using isOn or isOff 

simply 

(s at: 1 at: 1) isOn . 
(s at: 2 at: 1) isOn .  

added reset

(s reset) .

makes all squares to off

initially board is all off .

now find what is the solution to the problem 


s delete.

(50@50) 
cellsPerSide := 10 . 
cells := Matrix new: self cellsPerSide tabulate: [:i :j | (i @ j)].
cells. 


m := Matrix new: 10 tabulate: [:i :j | (i @ j)] .
m  at: 1 at: 1 . 
m  at: 1 at: 1 put: 5 . 
m tabulate [:i :j | (i @ j)] .



c := SBECell new openInWorld. 
c explore.


Rectangle new (0@0 corner: 50@50) className. 

s := SBEGame new openInWorld.
s bounds: (100@100 corner: (100 + (10 * 32))@(100 + (10 * 32))). 
s isSolved.
s reset. 
s toggleNeighboursOfCellAt: 1 at: 1 . 
s toggleNeighboursOfCellAt: 2 at: 1 .
s toggleNeighboursOfCellAt: 10 at: 1 .

(s at: 1 at: 1) isOn . 
(s at: 2 at: 1) isOn .  

s delete.

(50@50) 
cellsPerSide := 10 . 
cells := Matrix new: self cellsPerSide tabulate: [:i :j | (i @ j)].
cells. 


m := Matrix new: 10 tabulate: [:i :j | (i @ j)] .
m  at: 1 at: 1 . 
m  at: 1 at: 1 put: 5 . 
m tabulate [:i :j | (i @ j)] .


