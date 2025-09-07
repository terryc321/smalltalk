Class {
	#name : 'SokobanBox',
	#superclass : 'ImageMorph',
	#instVars : [
		'x',
		'y',
		'transition',
		'counter',
		'form'
	],
	#category : 'PBE-Sokoban',
	#package : 'PBE-Sokoban'
}

{ #category : 'as yet unclassified' }
SokobanBox >> counter [ 
 ^ counter . 




]

{ #category : 'as yet unclassified' }
SokobanBox >> createImage: parent [
 form := 3 .








]

{ #category : 'as yet unclassified' }
SokobanBox >> decrementCounter [ 
 counter := counter - 1 . 
 counter < 0 ifTrue: [ counter := 0  ] 






]

{ #category : 'as yet unclassified' }
SokobanBox >> inTransition [
 ^ transition isNotNil .


]

{ #category : 'initialization' }
SokobanBox >> initialize [ 
 super initialize . 
 x := 1 . 
 y := 1 .
 self image: self myDefaultForm .
 self updateScreenPosition .  




 



 

]

{ #category : 'as yet unclassified' }
SokobanBox >> isAbove: aBox [
 ^ y > aBox y .







]

{ #category : 'as yet unclassified' }
SokobanBox >> isAboveOneOf: aBox [
 ^ (y - (aBox y)) = -1 .










]

{ #category : 'as yet unclassified' }
SokobanBox >> isAboveTwoOf: aBox [
 ^ (y - (aBox y)) = -2 .










]

{ #category : 'as yet unclassified' }
SokobanBox >> isBelow: aBox [
 ^ y < aBox y .







]

{ #category : 'as yet unclassified' }
SokobanBox >> isBelowOneOf: aBox [
 ^ (y - (aBox y)) = 1 .










]

{ #category : 'as yet unclassified' }
SokobanBox >> isBelowTwoOf: aBox [
 ^ (y - (aBox y)) = 2 .










]

{ #category : 'as yet unclassified' }
SokobanBox >> isLeftOneOf: aBox [
 ^ (x - (aBox x)) = -1 .










]

{ #category : 'as yet unclassified' }
SokobanBox >> isLeftTwoOf: aBox [
 ^ (x - (aBox x)) = -2 .










]

{ #category : 'as yet unclassified' }
SokobanBox >> isRightOneOf: aBox [
 ^ (x - (aBox x)) = 1 .










]

{ #category : 'as yet unclassified' }
SokobanBox >> isRightTwoOf: aBox [
 ^ (x - (aBox x)) = 2 .










]

{ #category : 'as yet unclassified' }
SokobanBox >> localX [
 ^ x 

]

{ #category : 'as yet unclassified' }
SokobanBox >> localXY [
 ^ (x @ y)  .



]

{ #category : 'as yet unclassified' }
SokobanBox >> localY [
 ^ y 





]

{ #category : 'as yet unclassified' }
SokobanBox >> moveDown [ 
 y := y + 1 . 
 self updateScreenPosition .  
 







]

{ #category : 'as yet unclassified' }
SokobanBox >> moveLeft [ 
 x := x - 1 .
 self updateScreenPosition .  







]

{ #category : 'as yet unclassified' }
SokobanBox >> moveRight [
 x := x + 1 .
 self updateScreenPosition .  









]

{ #category : 'as yet unclassified' }
SokobanBox >> moveUp [ 
 y := y - 1 .
 self updateScreenPosition .  






]

{ #category : 'as yet unclassified' }
SokobanBox >> myDefaultForm [ 
 ^ (Form fromFileNamed: '/home/terry/code/smalltalk/pbe-sokoban/images/' , 'crate-100x100.jpeg').


 




]

{ #category : 'as yet unclassified' }
SokobanBox >> setLocalPosXY: aPos [
 x := aPos x .
 y := aPos y . 



]

{ #category : 'as yet unclassified' }
SokobanBox >> setTransitionFrom: aPos to: aPos2 [ 
 transition := #(aPos aPos2) . 





]

{ #category : 'as yet unclassified' }
SokobanBox >> updateScreenPosition [
 self position: (x*100)@(y*100) . 


]

{ #category : 'as yet unclassified' }
SokobanBox >> x [
 ^ x 

]

{ #category : 'as yet unclassified' }
SokobanBox >> y [
 ^ y 

]
