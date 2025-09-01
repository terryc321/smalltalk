
"TEC 1st Sept 2025"
"Squeak6.0-22148-64bit.1.image"
"Make a test morph that implements step and stepTime methods"
""

Morph subclass: #TestMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TEC-Experiments'

TestMorph >> step
 self color: Color random . 
 Transcript show: 'tick' ; cr .

TestMorph >> stepTime
  "milliseconds"
  ^ 500 . 

"In workspace we can test and see generic morph reports true to wantsSteps"
"we see a square changing color twice a second"
t := TestMorph new openInWorld.
t wantsSteps.  "true" 


"We can control if we want to animate by changing reply wantsSteps method "
" ^ true . want animation"
" ^ false . do not want animation - or more correctly code in step method to run "
TestMorph >> wantsSteps
 ^ true .



Morph browse. 
" From Morph class browser find method called wantsSteps " 
wantsSteps
	"Return true if the receiver overrides the default Morph step method."
	"Details: Find first class in superclass chain that implements #step and return true if it isn't class Morph."

	| c |
	self isPartsDonor ifTrue: [^ false].
	(self == self topRendererOrSelf) ifTrue: [self player wantsSteps ifTrue: [^ true]].
	c := self class.
	[c includesSelector: #step] whileFalse: [c := c superclass].
	^ c ~= Morph
