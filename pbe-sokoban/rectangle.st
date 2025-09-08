

"This makes a rectangle"

Rectangle origin:0@0 corner:50@50 .


drawOn: aCanvas
| colors |
colors := color wheel: 6.
colors withIndexDo: [:c :i |
aCanvas fillOval: (self bounds insetBy: 4 * i) color: c
].
aCanvas fillRectangle: (Rectangle origin:((0@0) + (100@200)) corner:((100@100) + (100@200))) color: Color red . 


=== ===
"$_ empty"
"$x wall"
"$a player"
"$b box"
Sokoban compile: 'drawOn: aCanvas
 | val c ownerPos px py px2 py2 origin corner |
 ownerPos := self position . 
 self position: 0@0 . 
 self extent: ((width+3) * 100) @ ((height +3) * 100) .
 aCanvas fillRectangle: (Rectangle origin: 0@0 corner: (((width+3) * 100) @ ((height +3) * 100))) color: Color black. 
 1 to: width do: [ :x |
  1 to: height do: [ :y |
	   "scale"
	   px := (x * 100). 
	   py := (y * 100). 
	   px2 := (px + 90) . 
	   py2 := (py + 90) . 
		"adjust"
		""
	   origin := px @ py . 
	   corner := px2 @ py2 . 
      val := board at: (x@y).
      val = $_ ifTrue: [ c := Color green ].
      val = $a ifTrue: [ c := Color blue].
      val = $x ifTrue: [ c := Color gray].
      val = $b ifTrue: [ c := Color orange].
      aCanvas fillRectangle: (Rectangle origin: origin corner: corner) 
              color: c ]].'
	classified: 'drawing'.





how do we pass multiple arguments to a closure ?
[:c :i | ... ]  how do we call this
value1: a value2: b ....


Clipping - means will not see shape 
