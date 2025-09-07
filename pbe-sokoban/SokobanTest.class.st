Class {
	#name : 'SokobanTest',
	#superclass : 'TestCase',
	#category : 'PBE-Sokoban',
	#package : 'PBE-Sokoban'
}

{ #category : 'tests' }
SokobanTest >> testClearBorderedMorphs [
  BorderedMorph allInstancesDo: [ :box | box delete ].




]

{ #category : 'tests' }
SokobanTest >> testClearBoxes [
  SokobanBox allInstancesDo: [ :box | box delete ].




]

{ #category : 'tests' }
SokobanTest >> testClearGames [
  SokobanGame allInstancesDo: [ :o | o delete ].





]

{ #category : 'tests' }
SokobanTest >> testClearMorphs [
  Morph allInstancesDo: [ :box | box delete ].




]

{ #category : 'tests' }
SokobanTest >> testIsLeftOneOf [
 | g m11 m12 m13 m21 m22 m23 m31 m32 m33 |
" m11 m21 m31"
" m12 m22 m32"
" m13 m23 m33"
 g := SokobanGame new.
 m11 := g newBoxAt: 1 by: 1 .
 m21 := g newBoxAt: 2 by: 1 .
 m31 := g newBoxAt: 3 by: 1 .
 m12 := g newBoxAt: 1 by: 2 .
 m22 := g newBoxAt: 2 by: 2 .
 m32 := g newBoxAt: 3 by: 2 .
 m13 := g newBoxAt: 1 by: 3 .
 m23 := g newBoxAt: 2 by: 3 .
 m33 := g newBoxAt: 3 by: 3 .

 #(m11 m12 m13 m21 m22 m23 m31 m32 m33) do: [ :e | 
	e = m32 ifFalse: [ 
		self assert: (m22 isLeftOneOf: e) not ] ] .

 self assert: ( m22 isLeftOneOf: m32 ) . 
 g delete.

 



]

{ #category : 'tests' }
SokobanTest >> testNewBox [
  | s | 
  s := SokobanBox new openInWorld. 


]

{ #category : 'tests' }
SokobanTest >> testNewGame [
"sokoban game is a BorderedMorph"
| m | 
m := SokobanGame new openInWorld. 
m position: 100@100 . 
"m color: Color black."
"m color: Color red. "
"m color: Color green."

]

{ #category : 'tests' }
SokobanTest >> testNewImage [
"SokobanPlayeris a ImageMorph"
"how do we set image from a file "
"what connection between an ImageForm and ImageMorph "
"background"
| playerImageMorph imageForm fileSystem imageDirectory | 
"why not just have advent morph be the containing morph - can image morph respond to keyboard focus "
fileSystem := Object new.
imageDirectory := fileSystem imageDirectory .
(ImageReadWriter formFromStream: (imageDirectory , 'warrior.png') asFileReference binaryReadStream) asMorph openInWindow. 


playerImageMorph := SokobanBox new openInWorld. 
playerImageMorph position: 400@400 . 
"imageForm := ZnEasy getPng: 'images/warrior.png'."
"imageForm inspect. "
"(ZnEasy getPng: 'http://pharo.org/web/files/pharo.png') asMorph openInWindow. "
(ImageReadWriter formFromStream: (imageDirectory , 'warrior.png') asFileReference binaryReadStream) asMorph openInWindow. 
(ImageReadWriter formFromStream: '/home/terry/code/smalltalk/pbe-sokoban/images/three-warriors.png' asFileReference binaryReadStream) asMorph openInWorld. 
(ImageReadWriter formFromStream: '/home/terry/code/smalltalk/pbe-sokoban/images/crate.jpeg' asFileReference binaryReadStream) asMorph openInWorld. 
(ImageReadWriter formFromStream: '/home/terry/code/smalltalk/pbe-sokoban/images/ninja.jpeg' asFileReference binaryReadStream) asMorph openInWorld. 
(ImageReadWriter formFromStream: '/home/terry/code/smalltalk/pbe-sokoban/images/advert.jpeg' asFileReference binaryReadStream) asMorph openInWorld. 

"CANNOT DO WEBP : huh : (ImageReadWriter formFromStream: '/home/terry/code/smalltalk/pbe-sokoban/images/guard.webp' asFileReference binaryReadStream) asMorph openInWorld. "

"asdf"


]

{ #category : 'tests' }
SokobanTest >> testNewPlayer [
  | s | 
  s := Object new.




]

{ #category : 'tests' }
SokobanTest >> testNewPlayer2 [
  | s | 
  s := SokobanBox new.


]
