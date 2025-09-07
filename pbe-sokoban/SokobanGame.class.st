"
Sokoban

: SIMPLEST THING : 
[ ] single 3 x 3 grid to move player around 

: ARGUMENTS : 
[ ] what is the argument for object orientated programming 

: LIMITATIONS :
[ ] temporary variables that may be needed in script
[ ] dust code with closures everywhere 
[ ] pharo views are not live - go to inspector on morph , move morph does not keep up with inspector
[ ] pharo view SokobanGame class out of date , switch tabs , eventually catches up 

: CODE EDITOR :
[ ] editor warnings for everything under sun just not helpful 
[ ] code editor main part where work gets done is awful
[ ] it just needs to ftfo 
[ ] editor loves itself too much 
   if delete a portion of window editor like maybe the category pane 
   it sometimes cannot save the code in the editor because of layout issues 
   its written as though everything will be there always and forever

: CONFUSING :
[ ] double quote for comments is bizarre 
[ ] resort to #( .... ) as stop gap for multi line comments
[ ] lack nested multiline comments is annoying

[ ] non repsonsive terminal send data to it , does not show 
[ ] Transcript clear. does not clear transcript.
[ ] confirmed pharo transcripts do not clear.

: POTENTIALLY USEFUL : 
[ ] timeToRun from  https://jiaxianhua.github.io/smalltalk/2015/05/01/pharo-by-example-10

: IMMEDIATE :
[ ] read a text file from file system
[ ] convert into 2 d grid 
[ ] for each entry in grid x be a box , space character ignore
[ ] construct board based off of latest loaded level


: TODO : 
[ ] Design and implement ONE single level 
[ ] Improvements to ONE level implementation 
[ ] How put into fullscreen mode
[ ] should make a new world and then enter that instead 
[ ] single class games 
[ ] read level from file and execute required stuff
[ ] turn smalltalk pharo into a scheme interpreter - can we add SDL3 instead
[ ] 

: ISSUES :
[ ] filestream is not defined in pharo


: COMPLETED :
[x] player controls a box using WASD or ARROW KEYS
[x] have a event loop of sorts
[x] have a rough clock of sorts
[x] aminmation is possible
[x] player has box image - albeit resized using gimp image manipulation program

============================================================================================

on a game like sokoban clearly follows rules , if in object orientated programming 
in wider sense if things get too out of control , 
surely reduce it to something like a theorem proof using propositional logic
feed in requirements , see if any conflicts , if not 
dump the code out the end and never touch it 

quality assurance QA

============================================================================================


2d grid game 
lets say this is a level
xxxxxxxxx
xo      x
x  a    x
x       x
x       x
x   b  cx
xxxxxxxxx

whilst on screen it will be concerned with pixels from our perspective a box is 100x100 pixels for now
read file from disk and dump a table like one above 


open levels directory , read level 1 .txt 
decipher that players box at top left
there is a crate b to move to location c
where upon level is complete
possibly with a time limit and a score board 
leader board etc..






Sokoban game is BorderedMorph

keyboard input is disappointing 
[a] limited to operating system keyboard shortcuts
[b] limited from knowing shift ctrl keys
[c] notorious keyboard delay as though typing which is horrible for games
 even a typing test game 
[d] is it fast enough for a game loop ? 


SokobanBox is an ImageMorph , we do not know how to resize it so we go outside and do it manually in gimp
100 x 100 image , representing a crate 

We can move crate around , use a box collision to determine if box is going to go outside playfield and 
contain it 
bit like pong
Really once have a good keyboard inpuit / response / some sprites and collision detection , 
some kind of timing , sprite maps , 
sound ? 
effectively have everything for making various games 

sudoku solvers 
sokoban - move crates to a target location 
pong 
pinball 




SokobanPlayer SokobanTarget SokobanWall SokobanBox are all ImageMorphs
hopefully all i need to do is figure out how to move them around on screen
and i have a sokoban game


```

 | keyValue |
 keyValue := anEvent keyValue .
 Transcript show: 'key down => ' ; show: keyValue ; cr .



handlesKeyDown: anEvent 
  ^ true

handleKeyDown: anEvent
| keyValue x y x2 y2 wx wy wx2 wy2 |
   "" wx wy window x y ""
   "" x y position of ""
   ""how do ""
    ""Transcript show: 'key stroke event '; cr .""
    keyValue := anEvent keyValue.
    ""keyValue inspect.""
    "" f = 102 F=70 ""
    "" w = 119  W=87 ""
    "" a = 97  a=65 ""
    "" s = 115  S=83 ""
    "" d = 100  D=68 ""
    wx := self position x .
    wy := self position y .
    x := boxes position x .
    y := boxes position y .
    x2 := boxes extent x .
    y2 := boxes extent y . 
    wx2 := self extent x .
    wy2 := self extent y . 
    keyValue = 100 ifTrue:[ x := x + 10 ] . ""right""
    keyValue = 119 ifTrue:[ y := y - 10  ] . ""up""
    keyValue = 97 ifTrue:[ x := x - 10 ] . ""left""
    keyValue = 115 ifTrue:[ y := y + 10 ] . ""down""
    x < wx ifTrue: [ x := wx ] .
    y < wy ifTrue: [ y := wy ] .
    (x + x2 )> (wx + wx2 ) ifTrue: [ x := wx + wx2 - x2  ] .
    (y + y2 )> (wy + wy2 ) ifTrue: [ y := wy + wy2 - y2  ] .
    boxes position: x @ y . 

    ""anEvent inspect.""
    keyValue = Character arrowLeft 
        ifTrue: [self position: self position - (0 @ 10).
	   Transcript show: 'up arrow pressed' ; cr . 
	].
    keyValue = 31 ""down arrow""
        ifTrue: [self position: self position + (0 @ 10).
		   Transcript show: 'down arrow pressed' ; cr . 
	].
    keyValue = 29 ""right arrow""
        ifTrue: [self position: self position + (10 @ 0).
		   Transcript show: 'right arrow pressed' ; cr . 
	].
    keyValue = 28 ""left arrow""
        ifTrue: [self position: self position - (10 @ 0).
		   Transcript show: 'left arrow pressed' ; cr . 
	]

```


a typical image morph

```
initialize 
	""comment stating purpose of instance-side method""
	""scope: class-variables  &  instance-variables""
 super initialize.
 self image: (self myDefaultForm).
 self setBalloonText: '...'.
```

```
myDefaultForm
 ""return a form presumably an image of some sort""
 ""Original file: imagecodericon.png""
| fileSystem imageDirectory form |
fileSystem := SokobanFileSystem new.
imageDirectory := fileSystem imageDirectory .
form := (ImageReadWriter formFromStream: (imageDirectory , 'warrior.png') asFileReference binaryReadStream).
^ form . 
```

there is another way to load a file from directory using fromFileNamed

```
myDefaultForm 
	| fileSystem imageDirectory form |
	fileSystem := SokobanFileSystem new.
	imageDirectory := fileSystem imageDirectory .
	
	""form := (ImageReadWriter formFromStream: (imageDirectory , 'crate.jpeg') asFileReference binaryReadStream).""
	
	""the method suggested by ImageMorph class""
	form := (Form fromFileNamed: imageDirectory , 'crate-100x100.jpeg').	
	""had to do a manual gimp image resize to 100x100 could not see how to resize crate image""
	""form := (Form fromFileNamed: imageDirectory , 'crate.jpeg').	""
		
   form scaledToSize: 100@100 .
	^ form . 


```

```
myDefaultForm
 ""return a form presumably an image of some sort""
 ""Original file: imagecodericon.png""
| fileSystem imageDirectory form |
fileSystem := SokobanFileSystem new.
imageDirectory := fileSystem imageDirectory .
form := (ImageReadWriter formFromStream: (imageDirectory , 'advert.jpeg') asFileReference binaryReadStream).
^ form . 

```


```
""rampant use of closures - without recursion ""
[ 
row := 1 .
col := 1 . 
1 to: (text size) do: [ :row |	 
	| col | 
	1 to: (text at: row) do: [  
		] 
	ch := text at: i . 
	#(Transcript show: ch ; show: ' -> at index -> ' ; show: i ; cr .).
	
	col := col + 1 	] .
] value .
""text doWithIndex: [ :e | [ :idx | Transcript show e ; show ' at index : ' ; show idx ; cr ] ].""
""dict := Dictionary new.""
"" somehow iterate over stream until the end  , figure out shape of board ""
""^ dict . ""

```


```
Character cr . 
Character space . 
self stopStepping.
self readAndDumpGrid .
Transcript show: 'hello world ' ; cr . 
Transcript clear.
```

```
     [ 'hello'  ] value. 
     [ |tmp| tmp := 3 . tmp ] value . 

     ^  ([ | dict | dict := Dictionary new. ] value ). 
```


```
pharo will not let this go 

 #(
 ""return a grid squares of where boxes need to go ""
 ""assume the border ""
 ) 

```


typical dictionary use
```
| d | 
d := Dictionary new.
d at: #a put: 'fred'. 
d at: #a ifAbsent: nil. 
```

test replies no meaning there is something at #a 
```
| d | 
d := Dictionary new.
d at: #a put: 'fred'. 
(d at: #a ifAbsent: nil) isNil ifTrue:[ 'yes' ] ifFalse:['no']. 

```


recap on keydown event handler
``` 
handleKeyDown: anEvent
    | keyValue x y x2 y2 wx wy wx2 wy2 |
   "" wx wy window x y ""
   "" x y position of ""
   ""how do ""
    ""Transcript show: 'key stroke event '; cr .""
    keyValue := anEvent keyValue.
    ""keyValue inspect.""
    "" f = 102 F=70 ""
    "" w = 119  W=87 ""
    "" a = 97  a=65 ""
    "" s = 115  S=83 ""
    "" d = 100  D=68 ""
    wx := self position x .
    wy := self position y .
    x := player position x .
    y := player position y .
    x2 := player extent x .
    y2 := player extent y . 
    wx2 := self extent x .
    wy2 := self extent y . 
    keyValue = 30 ifTrue: [ Transcript show: 'you pressed an arrow key ' ; cr ] . 
    keyValue = 100 ifTrue:[ x := x + 10 ] . ""right""
    keyValue = 119 ifTrue:[ y := y - 10  ] . ""up""
    keyValue = 97 ifTrue:[ x := x - 10 ] . ""left""
    keyValue = 115 ifTrue:[ y := y + 10 ] . ""down""
    x < wx ifTrue: [ x := wx ] .
    y < wy ifTrue: [ y := wy ] .
    (x + x2 )> (wx + wx2 ) ifTrue: [ x := wx + wx2 - x2  ] .
    (y + y2 )> (wy + wy2 ) ifTrue: [ y := wy + wy2 - y2  ] .
    player position: x @ y . 

    ""anEvent inspect.""
    keyValue = Character arrowLeft 
        ifTrue: [self position: self position - (0 @ 10).
	   Transcript show: 'up arrow pressed' ; cr . 
	].
    keyValue = 31 ""down arrow""
        ifTrue: [self position: self position + (0 @ 10).
		   Transcript show: 'down arrow pressed' ; cr . 
	].
    keyValue = 29 ""right arrow""
        ifTrue: [self position: self position + (10 @ 0).
		   Transcript show: 'right arrow pressed' ; cr . 
	].
    keyValue = 28 ""left arrow""
        ifTrue: [self position: self position - (10 @ 0).
		   Transcript show: 'left arrow pressed' ; cr . 
	]


```

```
| p |
player position . 
player extent. 
p := model at: #playerPos ifAbsent: nil . 
self manualLevelOne . 
self stopStepping.
(1@1) < (2@3)
""setup board""
self submorphsDo: [ :o | o delete ] .

| d | 
d := Dictionary new.
d at: #a put: 'fred'. 
(d at: #a ifAbsent: nil) isNil ifTrue:[ 'yes' ] ifFalse:['no']. 

[ 'hello'  ] value. 
[ |tmp| tmp := 3 . tmp ] value . 

Character cr . 
Character space . 
self stopStepping.
self readAndDumpGrid .
Transcript show: 'hello world ' ; cr . 
Transcript clear.
```


model is a dictionary , just going to do this by hand ...

```
model at: (1@1) put: (self newBoxAt: 1 by: 1) . 
model at: (2@1) put: (self newBoxAt: 2 by: 1) . 
model at: (3@1) put: (self newBoxAt: 3 by: 1) . 
model at: (4@1) put: (self newBoxAt: 4 by: 1) . 
model at: (5@1) put: (self newBoxAt: 5 by: 1) . 

model at: (1@1) put: (self newBoxAt: 1 by: 1) . 
model at: (1@2) put: (self newBoxAt: 1 by: 2) . 
model at: (1@3) put: (self newBoxAt: 1 by: 3) . 
model at: (1@4) put: (self newBoxAt: 1 by: 4) . 
model at: (1@5) put: (self newBoxAt: 1 by: 5) . 

model at: (5@1) put: (self newBoxAt: 5 by: 1) . 
model at: (5@2) put: (self newBoxAt: 5 by: 2) . 
model at: (5@3) put: (self newBoxAt: 5 by: 3) . 
model at: (5@4) put: (self newBoxAt: 5 by: 4) . 
model at: (5@5) put: (self newBoxAt: 5 by: 5) . 

model at: (1@5) put: (self newBoxAt: 1 by: 1) . 
model at: (2@5) put: (self newBoxAt: 2 by: 2) . 
model at: (3@5) put: (self newBoxAt: 3 by: 3) . 
model at: (4@5) put: (self newBoxAt: 4 by: 4) . 
model at: (5@5) put: (self newBoxAt: 5 by: 5) . 

```




"
Class {
	#name : 'SokobanGame',
	#superclass : 'BorderedMorph',
	#instVars : [
		'player',
		'boxes'
	],
	#category : 'PBE-Sokoban',
	#package : 'PBE-Sokoban'
}

{ #category : 'initialization' }
SokobanGame >> canGoDown [
 | p box1 box2  |
 "introspection look at the model to see if transition is possible"

 "if still in a transition moving boxes say no "
 "self inTransition ifTrue: [ ^ false ] ."

"x|x|x|x|x"
"x|o|_|_|x"
"x|_|b|_|x"
"x|_|_|_|x"
"x|x|x|x|x"

"p at playerPos"
p := player localXY .
box1 := model at: (p x) @ ((p y) + 1) ifAbsent: nil . "one above"
box2 := model at: (p x) @ ((p y) + 2) ifAbsent: nil . "two above"

(p y) > 7 ifTrue:[ ^ false ] . 

"nobody above us - put player into transition "
box1 isNil ifTrue:[ 
	"player setTransitionFrom: p to: (p x @ (p y) - 1) . "
	player moveDown . 	
	^ true ] . 

" boxAbove not nil "
" move two objects player and boxAbove "
box2 isNil ifTrue:[ 
	"player setTransitionFrom: p to: (p x @ (p y) - 1) ."
	player moveDown .   
	box1 moveDown . 
	
	
	] . "transition" .

"out of luck"
^false


]

{ #category : 'initialization' }
SokobanGame >> canGoLeft [
 | p box1 box2  |
 "introspection look at the model to see if transition is possible"

 "if still in a transition moving boxes say no "
 "self inTransition ifTrue: [ ^ false ] ."

"x|x|x|x|x"
"x|o|_|_|x"
"x|_|b|_|x"
"x|_|_|_|x"
"x|x|x|x|x"

"p at playerPos"
p := player localXY .
box1 := model at: ((p x) - 1) @ (p y) ifAbsent: nil . "one above"
box2 := model at: ((p x) - 2) @ (p y) ifAbsent: nil . "two above"

(p x) < 1 ifTrue:[ ^ false ] . 

"nobody above us - put player into transition "
box1 isNil ifTrue:[ 
	"player setTransitionFrom: p to: (p x @ (p y) - 1) . "
	player moveLeft . 	
	^ true ] . 

" boxAbove not nil "
" move two objects player and boxAbove "
box2 isNil ifTrue:[ 
	"player setTransitionFrom: p to: (p x @ (p y) - 1) ."
	player moveLeft .   
	box1 moveLeft . 
	
	
	] . "transition" .

"out of luck"
^false

]

{ #category : 'initialization' }
SokobanGame >> canGoRight [
 | p box1 box2  |
 "introspection look at the model to see if transition is possible"

 "if still in a transition moving boxes say no "
 "self inTransition ifTrue: [ ^ false ] ."

"x|x|x|x|x"
"x|o|_|_|x"
"x|_|b|_|x"
"x|_|_|_|x"
"x|x|x|x|x"

"p at playerPos"
p := player localXY .
box1 := model at: ((p x) + 1) @ (p y) ifAbsent: nil . "one above"
box2 := model at: ((p x) + 2) @ (p y) ifAbsent: nil . "two above"

"LIMITATION width 10 items "
(p x) > 7 ifTrue:[ ^ false ] . 

"nobody above us - put player into transition "
box1 isNil ifTrue:[ 
	"player setTransitionFrom: p to: (p x @ (p y) - 1) . "
	player moveRight . 	
	^ true ] . 

" boxAbove not nil "
" move two objects player and boxAbove "
box2 isNil ifTrue:[ 
	"player setTransitionFrom: p to: (p x @ (p y) - 1) ."
	player moveRight .   
	box1 moveRight 	] . "transition" .

"out of luck"
^false


]

{ #category : 'initialization' }
SokobanGame >> canGoUp [
 | p box1 box2  |
 "introspection look at the model to see if transition is possible"

 "if still in a transition moving boxes say no "
 "self inTransition ifTrue: [ ^ false ] ."

"x|x|x|x|x"
"x|o|_|_|x"
"x|_|b|_|x"
"x|_|_|_|x"
"x|x|x|x|x"

"p at playerPos"
p := player localXY .
boxes do: [ :e | e isAbove: player ifTrue: [ box1 := e ] .
	              e isAbove2: player ifTrue: [ box2 := e ] . ].

#(box1 := model at: (p x) @ ((p y) - 1) ifAbsent: nil . "one above"
  box2 := model at: (p x) @ ((p y) - 2) ifAbsent: nil . "two above"
).


(p y) < 1 ifTrue:[ ^ false ] . 

"nobody above us - put player into transition "
"player setTransitionFrom: p to: (p x @ (p y) - 1) . "
box1 isNil ifTrue:[ 	player moveUp . ^ true ] . 

" boxAbove not nil "
" move two objects player and boxAbove "
	"player setTransitionFrom: p to: (p x @ (p y) - 1) ."
box2 isNil ifTrue:[ 	player moveUp .  	box1 moveUp ] . "transition" .

"out of luck"
^false

]

{ #category : 'initialization' }
SokobanGame >> canTransition [
 ^ transition

]

{ #category : 'initialization' }
SokobanGame >> handleKeyDown: anEvent [
    | keyValue |
    keyValue := anEvent keyValue.

 "   keyValue = 100 ifTrue: [ Transcript show: ' d key' . ^ self canGoRight  ] ."



keyValue = 29 ifTrue: [ 	Transcript show: 'right arrow' . 	^ self canGoRight  ] .
keyValue = 28 ifTrue: [ 	Transcript show: 'left arrow' . 	^ self canGoLeft  ] .
keyValue = 31 ifTrue: [ 	Transcript show: 'down arrow' . 	^ self canGoDown  ] .
keyValue = 30 ifTrue: [ 	Transcript show: 'up arrow' . 	^ self canGoUp  ] .




]

{ #category : 'initialization' }
SokobanGame >> handleKeystroke: anEvent [
    ^ self

]

{ #category : 'initialization' }
SokobanGame >> handlesKeyDown:  anEvent [
 ^ true 

]

{ #category : 'initialization' }
SokobanGame >> handlesMouseOver: anEvent [
    ^true
]

{ #category : 'file paths' }
SokobanGame >> imagesDirectory [ 
 "localise all game images in this directory"
  ^ self rootDirectory , 'images/' 


]

{ #category : 'initialization' }
SokobanGame >> inTransition [ 
 "any submorphs still in transition "
 self submorphsDo: [ :o | o inTransition ifTrue:[ ^ true ] ] .
 ^ false . 



]

{ #category : 'initialization' }
SokobanGame >> initialize [ 	
 super initialize.

 
 self color: Color black . 
 
 self position: 0@0 . 
 self extent: 600@600. 
 
 self openInWorld.

 " self startStepping."
" start level one "
 self manualLevelOne .








 
]

{ #category : 'file paths' }
SokobanGame >> levelsDirectory [ 
 "localise all game images in this directory"
  ^ self rootDirectory , 'levels/' 


]

{ #category : 'initialization' }
SokobanGame >> manualLevelOne [
 | wid hgt |
 wid := 5 .
 hgt := 5 . 


"clear away all submorphs"
self submorphsDo: [ :o | o delete ] .


"we are going to set the grid manually - p[lace everything , disallow movements "


"x|x|x|x|x"
"x|o|_|_|x"
"x|_|b|_|x"
"x|_|_|_|x"
"x|x|x|x|x"
 "boxes"


boxes := OrderedCollection new. 
boxes add: (self newBoxAt: 2 by: 2 ). 


#(1 to: wid do: [ :x |
	1 to: hgt do: [  :y |
		| e | 		
		e := model at: (x@y) ifAbsent: nil .
		e isNil ifFalse:[ self addMorph: e inFrontOf: self ]					
		 ] ]).


"player is not a box ? "
player := self newBoxAt: 1 by: 1 . 

boxes do:[ :e | self addMorph: e inFrontOf: self ] . 
self addMorph: player inFrontOf: self. 

















]

{ #category : 'initialization' }
SokobanGame >> mouseEnter: anEvent [
    anEvent hand newKeyboardFocus: self.
    Transcript show:' mouse entered!' ; cr.



]

{ #category : 'initialization' }
SokobanGame >> mouseLeave: anEvent [
    anEvent hand newKeyboardFocus: nil.
    Transcript show:' mouse left - keyboard focus gone' ; cr.




]

{ #category : 'initialization' }
SokobanGame >> newBoxAt: x by: y [ 
 | morph | 
 morph := SokobanBox new.
 morph setLocalPosXY: (x@y) .
 morph updateScreenPosition .
 "morph createImage: self. "
 ^ morph .




















]

{ #category : 'tests - todo' }
SokobanGame >> readAndDumpGrid [
 "lets say the grid is located "
| stream text lines | 
stream := ((self levelsDirectory) , 'level3.txt') asFileReference readStream. 
text := stream contents.
Transcript clear.
Transcript show: text ; cr . 

#( "no idea what this is about "
"split text based on lines"
lines := text substrings: ' ' . 

Transcript show: '===================' ; cr .
Transcript show: (lines at: 1) ; cr .
Transcript show: '===================' ; cr .
).








]

{ #category : 'file paths' }
SokobanGame >> rootDirectory [ 
 "localise all game images in this directory"
 ^ '/home/terry/code/smalltalk/pbe-sokoban/'




]

{ #category : 'file paths' }
SokobanGame >> step [ 
 | p |
 " p := player position."
 " player position: (p x) + 1 @ (p y) + 1  . "
 self submorphsDo: [ :o | o updateScreenPosition ] .




 

]

{ #category : 'file paths' }
SokobanGame >> stepTime [
 "milliseconds"
 ^ 10

]

{ #category : 'file paths' }
SokobanGame >> wantsSteps [  
 ^ true 

]
