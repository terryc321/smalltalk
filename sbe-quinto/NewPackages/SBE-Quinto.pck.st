'From Cuis7.5 [latest update: #7471] on 31 August 2025 at 2:05:39 pm'!
'Description '!
!provides: 'SBE-Quinto' 1 1!
SystemOrganization addCategory: #'SBE-Quinto'!


!classDefinition: #SBECell category: #'SBE-Quinto'!
ProtoObject subclass: #SBECell
	instanceVariableNames: 'mouseAction'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SBE-Quinto'!
!classDefinition: 'SBECell class' category: #'SBE-Quinto'!
SBECell class
	instanceVariableNames: ''!

!classDefinition: #SBEGame category: #'SBE-Quinto'!
ProtoObject subclass: #SBEGame
	instanceVariableNames: 'cells dict'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SBE-Quinto'!
!classDefinition: 'SBEGame class' category: #'SBE-Quinto'!
SBEGame class
	instanceVariableNames: ''!


!SBEGame commentStamp: 'tc 8/31/2025 10:06' prior: 0!
cells contain SBECell which are SimpleSwitchMorph . dict is a dictionary that holds key is grid location like (1,1) would be upper left square , (10,10) would be lower right square for a 10 cells per side .  then to query if cell (2,3) is turned on or off we look in the dict for position (2@3) and should find a cell , then call isOn we can tell if it is on or off.!

!SBECell methodsFor: 'as yet unclassified' stamp: 'tc 8/31/2025 09:23'!
mouseAction: aBlock
mouseAction := aBlock! !

!SBECell methodsFor: 'initialization' stamp: 'tc 8/31/2025 09:48'!
initialize 
super initialize.
self
	label: '' ;
	borderWidth: 2 ;
	bounds: (0@0 corner: 32@32); 
	offColor: Color paleYellow ;
	onColor: Color paleBlue darker ;
	useSquareCorners.
self turnOff .! !

!SBECell methodsFor: 'event handling' stamp: 'tc 8/31/2025 09:29'!
mouseUp: anEvent
 mouseAction value


! !

!SBEGame methodsFor: 'initialization' stamp: 'tc 8/31/2025 10:12'!
at: x at: y
 ^ cells at: x at: y .

! !

!SBEGame methodsFor: 'initialization' stamp: 'tc 8/31/2025 09:21'!
cellsPerSide
"The number of cells along each side of the game"
^ 10

! !

!SBEGame methodsFor: 'initialization' stamp: 'tc 8/31/2025 09:19'!
initialize 
| height overallBorderWidth sampleCell width |
super initialize.
sampleCell := SBECell new.
width := sampleCell width.
height := sampleCell height.
overallBorderWidth := 2 * self borderWidth.
self position: 5 @ 5.
self extent: (width * self cellsPerSide) @ (height * self cellsPerSide) +
overallBorderWidth.
cells := Matrix new: self cellsPerSide tabulate: [:i :j | self newCellAt: i at: j].! !

!SBEGame methodsFor: 'initialization' stamp: 'tc 8/31/2025 10:14'!
reset
"turn all cells to off"
cells do: [:cell | cell turnOff ] .



! !

!SBEGame methodsFor: 'as yet unclassified' stamp: 'tc 8/31/2025 10:26'!
isSolved
 "is the game solved , meaning are all the cells ON . 
  if any cell is off then the game is not yet solved
  otherwise the game is completely solved
  is it solvable ? "
 cells do: [:cell | cell isOff ifTrue:[^ false]].
 ^ true.! !

!SBEGame methodsFor: 'as yet unclassified' stamp: 'tc 8/31/2025 09:32'!
newCellAt: i at: j
"Create a cell for position (i,j) and add it to my on-screen
representation at the appropriate screen position. Answer the new cell."
| cell origin |
cell := SBECell new.
origin := self innerBounds origin.
self addMorph: cell.
cell
position: ((i - 1) * cell width) @ ((j - 1) * cell height) + origin;
mouseAction: [self toggleNeighboursOfCellAt: i at: j].
^cell. 
! !

!SBEGame methodsFor: 'as yet unclassified' stamp: 'tc 8/31/2025 13:59'!
toggleNeighboursOfCellAt: i at: j
"left neighbour"
i > 1 ifTrue: [(cells at: i - 1 at: j) toggleState].

"right neighbour"
i < self cellsPerSide ifTrue: [(cells at: i + 1 at: j) toggleState].

"above neighbour"
j > 1 ifTrue: [(cells at: i at: j - 1) toggleState].

"below neighbour"
j < self cellsPerSide ifTrue: [(cells at: i at: j + 1) toggleState].! !
