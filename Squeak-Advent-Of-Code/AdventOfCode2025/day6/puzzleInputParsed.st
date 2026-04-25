'From Squeak6.0 of 14 January 2025 [latest update: #22154] on 25 April 2026 at 12:02:54 pm'!

!AOC2025Day6 methodsFor: 'as yet unclassified' stamp: 'tc 4/25/2026 11:20'!
puzzleInputParsed
	"1) split into lines. 2) split by space character 3) remove empty strings .
	input becomes 4 lines each with 1000 numbers
	and 1 line with 1000 math symbols either #+ addition or #* multiplication "

	| lines lines2 lines3 |
	lines := (self puzzleInput splitBy: String cr).
	lines2 := lines collect: [ :line | line splitBy: String space] .
	lines3 := OrderedCollection new.
	lines2 do: [ :line |
		| ns |
		ns := OrderedCollection new.
		line do: [ :str |
			| alldigits |
			alldigits := true.
			str do: [:e | (e isDigit = false) ifTrue:[alldigits := false] ].			(str = '') ifTrue: [alldigits := false]			              ifFalse: [
			alldigits ifTrue: [ns add: str asInteger]
			          ifFalse: [ns add: str asSymbol]]].				  
		lines3 add: ns ].
	^ lines3 .	
	! !

