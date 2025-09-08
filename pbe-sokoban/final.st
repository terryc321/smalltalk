
"BorderedBoxMorph for cuis perhaps"
"BorderedMorph for pharo"

BorderedMorph subclass: #Sokoban
       instanceVariableNames: 'width height board player'
       classVariableNames: ''
       package: 'PBE-Sokoban'.


" ==== everything below this line can be read directly from the workspace ==== "

Sokoban compile: 'initialize
    super initialize.
    self reset.'	
  classified: 'startup'.


Sokoban compile: 'reset
    self level1.'	
  classified: 'reset'.




"can i go left ,  "
" _ a   : if decide move left need to put player at x-1 move a from where it is , to where needs to be"
" _ b a  : if decide move left push box left also player at x-1 move a from where it is to , put b at x-2"
Sokoban compile: 'canGoLeft
| x y e f |
x := player x .
y := player y .
e := board at: ((x - 1) @ y) ifAbsent: nil.
f := board at: ((x - 2) @ y) ifAbsent: nil.
x >= 2 ifTrue: [ e = $_ ifTrue: [ ^ [ 
          board at: ((x - 1) @ y) put: $a .
          board at: (x @ y) put: $_ . 
          player := (x - 1 @ y).
]]].
x >= 3 ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ [ 
          board at: ((x - 2) @ y) put: $b .
	  board at: ((x - 1) @ y) put: $a .
	  board at: (x @ y) put: $_ .
          player := (x - 1 @ y).
]]]].
^ false'
 classified: 'ask'.

" ==========  =================== ============= ================= ===============  "

"can i go right - depends on size board and whats there "
Sokoban compile: 'canGoRight
| x y e f |
x := player x .
y := player y .
e := board at: ((x + 1) @ y) ifAbsent: nil.
f := board at: ((x + 2) @ y) ifAbsent: nil.
x <= (width - 1) ifTrue: [ e = $_ ifTrue: [ ^ [ 
          board at: ((x + 1) @ y) put: $a .
          board at: ( x @ y) put: $_ . 
          player := (x + 1 @ y).

]]].
x <= (width - 2) ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ [ 
          board at: ((x + 2) @ y) put: $b .
	  board at: ((x + 1) @ y) put: $a .
	  board at: (x @ y) put: $_ .
          player := (x + 1 @ y).
]]]].
^ false'
 classified: 'ask'.

" ==========  =================== ============= ================= ===============  "

Sokoban compile: 'canGoDown
| x y e f |
x := player x .
y := player y .
e := board at: (x @ (y + 1)) ifAbsent: nil.
f := board at: (x @ (y + 2)) ifAbsent: nil.
y <= (height - 1) ifTrue: [ e = $_ ifTrue: [ ^ [ 
          board at: (x @ (y + 1)) put: $a .          
          player := (x @ (y + 1)).
	  board at: (x @ y) put: $_ .   
]]].
y <= (height - 2) ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ [ 
          board at: (x @ (y + 2)) put: $b .
	  board at: (x @ (y + 1)) put: $a .
          player := (x @ (y + 1)).
	  board at: (x @ y) put: $_ .
]]]].
^ false'
 classified: 'ask'.

" ==========  =================== ============= ================= ===============  "

Sokoban compile: 'canGoUp
| x y e f |
x := player x .
y := player y .
e := board at: (x @ (y - 1)) ifAbsent: nil.
f := board at: (x @ (y - 2)) ifAbsent: nil.
y >= 2 ifTrue: [ e = $_ ifTrue: [ ^ [ 
          board at: (x @ (y - 1)) put: $a .
          player := (x @ (y - 1)).
          board at: (x @ y) put: $_ . 
]]].
y >= 3 ifTrue: [ e = $b ifTrue: [ f = $_ ifTrue: [^ [ 
          board at: (x @ (y - 2)) put: $b .
	  board at: (x @ (y - 1)) put: $a .
          player := (x @ (y - 1)).
	  board at: (x @ y) put: $_ .
]]]].
^ false'
 classified: 'ask'.



Sokoban compile: 'showBoard
 | val | 
  1 to: height do: [:y | 
    Transcript cr .
    1 to: width do: [:x | 
	val :=  board at: (x@y) .
       Transcript show: val ; show: '' '' ]] .
    Transcript cr . '
 classified: 'show'.


Sokoban compile: 'width
 "width of board"
 ^ width.'
 classified: 'accessors'.

Sokoban compile: 'height
 "height of board"
 ^ height.'
 classified: 'accessors'.

Sokoban compile: 'at: aPoint
 "what is at point on board x y"
 ^ board at: aPoint ifAbsent: nil .'
 classified: 'accessors'.

" === this section is visual confirmation but does not  ==== "

" use Ctrl D to do it - a text based interactive gui "
soko := Sokoban new. 
Transcript clear. soko showBoard .
soko reset. 

soko canGoLeft value. soko showBoard . 
soko canGoRight value. soko showBoard.  
soko canGoUp value. soko showBoard.  
soko canGoDown value. soko showBoard.  

soko inspect. 


" === how do we create a class on the fly through the workspace at runtime dynamically without ide === "
" === this applies to cuis / pharo / === "
" === how do we figure out what version of smalltalk we are using === "

" === here we add IDE ==="


Sokoban compile: 'handleKeystroke: anEvent
  ^ self.'
classified: 'actions'.

"cannot tell if shift ctrl or alt keys left or right are pressed "
"many keys will be caught by outside operating system even before we get here anyways"
Sokoban compile: 'handleKeyDown: anEvent
    | keyValue tr |
    tr := [ :message | Transcript show: message ; cr ] .
    keyValue := anEvent keyValue.
keyValue = 9 ifTrue: [ Transcript show: ''you pressed the tab   key '' ; cr ].
keyValue = 27 ifTrue: [ Transcript show: ''you pressed the escape key '' ; cr . 
      self reset].
keyValue = 28 ifTrue: [ Transcript show: ''you pressed the left arrow   key '' ; cr . 
    self canGoLeft value ].
keyValue = 29 ifTrue: [ Transcript show: ''you pressed the right arrow   key '' ; cr .
       self canGoRight value ].
keyValue = 30 ifTrue: [ Transcript show: ''you pressed the up arrow key '' ; cr . 
      self canGoUp value ].
keyValue = 31 ifTrue: [ Transcript show: ''you pressed the down arrow  key '' ; cr . 
       self canGoDown value ].
keyValue = 32 ifTrue: [ Transcript show: ''you pressed the spacebar  key '' ; cr ].
keyValue = 33 ifTrue: [ Transcript show: ''you pressed the ! key '' ; cr ].
keyValue = 34 ifTrue: [ Transcript show: ''you pressed the " key '' ; cr ].
keyValue = 35 ifTrue: [ Transcript show: ''you pressed the # key '' ; cr ].
keyValue = 36 ifTrue: [ Transcript show: ''you pressed the $ key '' ; cr ].
keyValue = 37 ifTrue: [ Transcript show: ''you pressed the % key '' ; cr ].
keyValue = 38 ifTrue: [ Transcript show: ''you pressed the & key '' ; cr ].
keyValue = 39 ifTrue: [ Transcript show: ''you pressed the apost  key '' ; cr ].
keyValue = 40 ifTrue: [ Transcript show: ''you pressed the ( key '' ; cr ].
keyValue = 41 ifTrue: [ Transcript show: ''you pressed the ) key '' ; cr ].
keyValue = 42 ifTrue: [ Transcript show: ''you pressed the * key '' ; cr ].
keyValue = 43 ifTrue: [ Transcript show: ''you pressed the + key '' ; cr ].
keyValue = 44 ifTrue: [ Transcript show: ''you pressed the , key '' ; cr ].
keyValue = 45 ifTrue: [ Transcript show: ''you pressed the - key '' ; cr ].
keyValue = 46 ifTrue: [ Transcript show: ''you pressed the . key '' ; cr ].
keyValue = 47 ifTrue: [ Transcript show: ''you pressed the / key '' ; cr ].
keyValue = 48 ifTrue: [ Transcript show: ''you pressed the 0 key '' ; cr ].
keyValue = 49 ifTrue: [ Transcript show: ''you pressed the 1 key '' ; cr ].
keyValue = 50 ifTrue: [ Transcript show: ''you pressed the 2 key '' ; cr ].
keyValue = 51 ifTrue: [ Transcript show: ''you pressed the 3 key '' ; cr ].
keyValue = 52 ifTrue: [ Transcript show: ''you pressed the 4 key '' ; cr ].
keyValue = 53 ifTrue: [ Transcript show: ''you pressed the 5 key '' ; cr ].
keyValue = 54 ifTrue: [ Transcript show: ''you pressed the 6 key '' ; cr ].
keyValue = 55 ifTrue: [ Transcript show: ''you pressed the 7 key '' ; cr ].
keyValue = 56 ifTrue: [ Transcript show: ''you pressed the 8 key '' ; cr ].
keyValue = 57 ifTrue: [ Transcript show: ''you pressed the 9 key '' ; cr ].
keyValue = 58 ifTrue: [ Transcript show: ''you pressed the : key '' ; cr ].
keyValue = 59 ifTrue: [ Transcript show: ''you pressed the ; key '' ; cr ].
keyValue = 60 ifTrue: [ Transcript show: ''you pressed the < key '' ; cr ].
keyValue = 61 ifTrue: [ Transcript show: ''you pressed the = key '' ; cr ].
keyValue = 62 ifTrue: [ Transcript show: ''you pressed the > key '' ; cr ].
keyValue = 63 ifTrue: [ Transcript show: ''you pressed the ? key '' ; cr ].
keyValue = 64 ifTrue: [ Transcript show: ''you pressed the @ key '' ; cr ].
keyValue = 65 ifTrue: [ Transcript show: ''you pressed the A key '' ; cr ].
keyValue = 66 ifTrue: [ Transcript show: ''you pressed the B key '' ; cr ].
keyValue = 67 ifTrue: [ Transcript show: ''you pressed the C key '' ; cr ].
keyValue = 68 ifTrue: [ Transcript show: ''you pressed the D key '' ; cr ].
keyValue = 69 ifTrue: [ Transcript show: ''you pressed the E key '' ; cr ].
keyValue = 70 ifTrue: [ Transcript show: ''you pressed the F key '' ; cr ].
keyValue = 71 ifTrue: [ Transcript show: ''you pressed the G key '' ; cr ].
keyValue = 72 ifTrue: [ Transcript show: ''you pressed the H key '' ; cr ].
keyValue = 73 ifTrue: [ Transcript show: ''you pressed the I key '' ; cr ].
keyValue = 74 ifTrue: [ Transcript show: ''you pressed the J key '' ; cr ].
keyValue = 75 ifTrue: [ Transcript show: ''you pressed the K key '' ; cr ].
keyValue = 76 ifTrue: [ Transcript show: ''you pressed the L key '' ; cr ].
keyValue = 77 ifTrue: [ Transcript show: ''you pressed the M key '' ; cr ].
keyValue = 78 ifTrue: [ Transcript show: ''you pressed the N key '' ; cr ].
keyValue = 79 ifTrue: [ Transcript show: ''you pressed the O key '' ; cr ].
keyValue = 80 ifTrue: [ Transcript show: ''you pressed the P key '' ; cr ].
keyValue = 81 ifTrue: [ Transcript show: ''you pressed the Q key '' ; cr ].
keyValue = 82 ifTrue: [ Transcript show: ''you pressed the R key '' ; cr ].
keyValue = 83 ifTrue: [ Transcript show: ''you pressed the S key '' ; cr ].
keyValue = 84 ifTrue: [ Transcript show: ''you pressed the T key '' ; cr ].
keyValue = 85 ifTrue: [ Transcript show: ''you pressed the U key '' ; cr ].
keyValue = 86 ifTrue: [ Transcript show: ''you pressed the V key '' ; cr ].
keyValue = 87 ifTrue: [ Transcript show: ''you pressed the W key '' ; cr ].
keyValue = 88 ifTrue: [ Transcript show: ''you pressed the X key '' ; cr ].
keyValue = 89 ifTrue: [ Transcript show: ''you pressed the Y key '' ; cr ].
keyValue = 90 ifTrue: [ Transcript show: ''you pressed the Z key '' ; cr ].
keyValue = 91 ifTrue: [ Transcript show: ''you pressed the [ key '' ; cr ].
keyValue = 92 ifTrue: [ Transcript show: ''you pressed the \ key '' ; cr ].
keyValue = 93 ifTrue: [ Transcript show: ''you pressed the ] key '' ; cr ].
keyValue = 94 ifTrue: [ Transcript show: ''you pressed the ^ key '' ; cr ].
keyValue = 95 ifTrue: [ Transcript show: ''you pressed the _ key '' ; cr ].
keyValue = 96 ifTrue: [ Transcript show: ''you pressed the ` key '' ; cr ].
keyValue = 97 ifTrue: [ Transcript show: ''you pressed the a key '' ; cr . 
   self canGoLeft value ].
keyValue = 98 ifTrue: [ Transcript show: ''you pressed the b key '' ; cr ].
keyValue = 99 ifTrue: [ Transcript show: ''you pressed the c key '' ; cr ].
keyValue = 100 ifTrue: [ Transcript show: ''you pressed the d key '' ; cr . 
    self canGoRight value ].
keyValue = 101 ifTrue: [ Transcript show: ''you pressed the e key '' ; cr ].
keyValue = 102 ifTrue: [ Transcript show: ''you pressed the f key '' ; cr ].
keyValue = 103 ifTrue: [ Transcript show: ''you pressed the g key '' ; cr ].
keyValue = 104 ifTrue: [ Transcript show: ''you pressed the h key '' ; cr ].
keyValue = 105 ifTrue: [ Transcript show: ''you pressed the i key '' ; cr ].
keyValue = 106 ifTrue: [ Transcript show: ''you pressed the j key '' ; cr ].
keyValue = 107 ifTrue: [ Transcript show: ''you pressed the k key '' ; cr ].
keyValue = 108 ifTrue: [ Transcript show: ''you pressed the l key '' ; cr ].
keyValue = 109 ifTrue: [ Transcript show: ''you pressed the m key '' ; cr ].
keyValue = 110 ifTrue: [ Transcript show: ''you pressed the n key '' ; cr ].
keyValue = 111 ifTrue: [ Transcript show: ''you pressed the o key '' ; cr ].
keyValue = 112 ifTrue: [ Transcript show: ''you pressed the p key '' ; cr ].
keyValue = 113 ifTrue: [ Transcript show: ''you pressed the q key '' ; cr ].
keyValue = 114 ifTrue: [ Transcript show: ''you pressed the r key '' ; cr ].
keyValue = 115 ifTrue: [ Transcript show: ''you pressed the s key '' ; cr. 
   self canGoDown value ].
keyValue = 116 ifTrue: [ Transcript show: ''you pressed the t key '' ; cr ].
keyValue = 117 ifTrue: [ Transcript show: ''you pressed the u key '' ; cr ].
keyValue = 118 ifTrue: [ Transcript show: ''you pressed the v key '' ; cr ].
keyValue = 119 ifTrue: [ Transcript show: ''you pressed the w key '' ; cr .
   self canGoUp value ].
keyValue = 120 ifTrue: [ Transcript show: ''you pressed the x key '' ; cr ].
keyValue = 121 ifTrue: [ Transcript show: ''you pressed the y key '' ; cr ].
keyValue = 122 ifTrue: [ Transcript show: ''you pressed the z key '' ; cr ].
keyValue = 123 ifTrue: [ Transcript show: ''you pressed the { key '' ; cr ].
keyValue = 124 ifTrue: [ Transcript show: ''you pressed the | key '' ; cr ].
keyValue = 125 ifTrue: [ Transcript show: ''you pressed the } key '' ; cr ].
keyValue = 126 ifTrue: [ Transcript show: ''you pressed the ~ key '' ; cr ].
keyValue = 161 ifTrue: [ Transcript show: ''you pressed the ¡ key '' ; cr ].
keyValue = 162 ifTrue: [ Transcript show: ''you pressed the ¢ key '' ; cr ].
keyValue = 163 ifTrue: [ Transcript show: ''you pressed the £ key '' ; cr ].
keyValue = 164 ifTrue: [ Transcript show: ''you pressed the ¤ key '' ; cr ].
keyValue = 165 ifTrue: [ Transcript show: ''you pressed the ¥ key '' ; cr ].
keyValue = 166 ifTrue: [ Transcript show: ''you pressed the ¦ key '' ; cr ].
keyValue = 167 ifTrue: [ Transcript show: ''you pressed the § key '' ; cr ].
keyValue = 168 ifTrue: [ Transcript show: ''you pressed the ¨ key '' ; cr ].
keyValue = 169 ifTrue: [ Transcript show: ''you pressed the © key '' ; cr ].
keyValue = 170 ifTrue: [ Transcript show: ''you pressed the ª key '' ; cr ].
keyValue = 171 ifTrue: [ Transcript show: ''you pressed the « key '' ; cr ].
keyValue = 172 ifTrue: [ Transcript show: ''you pressed the ¬ key '' ; cr ].
keyValue = 173 ifTrue: [ Transcript show: ''you pressed the ­ key '' ; cr ].
keyValue = 174 ifTrue: [ Transcript show: ''you pressed the ® key '' ; cr ].
keyValue = 175 ifTrue: [ Transcript show: ''you pressed the ¯ key '' ; cr ].
keyValue = 176 ifTrue: [ Transcript show: ''you pressed the ° key '' ; cr ].
keyValue = 177 ifTrue: [ Transcript show: ''you pressed the ± key '' ; cr ].
keyValue = 178 ifTrue: [ Transcript show: ''you pressed the ² key '' ; cr ].
keyValue = 179 ifTrue: [ Transcript show: ''you pressed the ³ key '' ; cr ].
keyValue = 180 ifTrue: [ Transcript show: ''you pressed the ´ key '' ; cr ].
keyValue = 181 ifTrue: [ Transcript show: ''you pressed the µ key '' ; cr ].
keyValue = 182 ifTrue: [ Transcript show: ''you pressed the ¶ key '' ; cr ].
keyValue = 183 ifTrue: [ Transcript show: ''you pressed the · key '' ; cr ].
keyValue = 184 ifTrue: [ Transcript show: ''you pressed the ¸ key '' ; cr ].
keyValue = 185 ifTrue: [ Transcript show: ''you pressed the ¹ key '' ; cr ].
keyValue = 186 ifTrue: [ Transcript show: ''you pressed the º key '' ; cr ].
keyValue = 187 ifTrue: [ Transcript show: ''you pressed the » key '' ; cr ].
keyValue = 188 ifTrue: [ Transcript show: ''you pressed the ¼ key '' ; cr ].
keyValue = 189 ifTrue: [ Transcript show: ''you pressed the ½ key '' ; cr ].
keyValue = 190 ifTrue: [ Transcript show: ''you pressed the ¾ key '' ; cr ].
keyValue = 191 ifTrue: [ Transcript show: ''you pressed the ¿ key '' ; cr ].
keyValue = 192 ifTrue: [ Transcript show: ''you pressed the À key '' ; cr ].
keyValue = 193 ifTrue: [ Transcript show: ''you pressed the Á key '' ; cr ].
keyValue = 194 ifTrue: [ Transcript show: ''you pressed the Â key '' ; cr ].
keyValue = 195 ifTrue: [ Transcript show: ''you pressed the Ã key '' ; cr ].
keyValue = 196 ifTrue: [ Transcript show: ''you pressed the Ä key '' ; cr ].
keyValue = 197 ifTrue: [ Transcript show: ''you pressed the Å key '' ; cr ].
keyValue = 198 ifTrue: [ Transcript show: ''you pressed the Æ key '' ; cr ].
keyValue = 199 ifTrue: [ Transcript show: ''you pressed the Ç key '' ; cr ].
keyValue = 200 ifTrue: [ Transcript show: ''you pressed the È key '' ; cr ].
keyValue = 201 ifTrue: [ Transcript show: ''you pressed the É key '' ; cr ].
keyValue = 202 ifTrue: [ Transcript show: ''you pressed the Ê key '' ; cr ].
keyValue = 203 ifTrue: [ Transcript show: ''you pressed the Ë key '' ; cr ].
keyValue = 204 ifTrue: [ Transcript show: ''you pressed the Ì key '' ; cr ].
keyValue = 205 ifTrue: [ Transcript show: ''you pressed the Í key '' ; cr ].
keyValue = 206 ifTrue: [ Transcript show: ''you pressed the Î key '' ; cr ].
keyValue = 207 ifTrue: [ Transcript show: ''you pressed the Ï key '' ; cr ].
keyValue = 208 ifTrue: [ Transcript show: ''you pressed the Ð key '' ; cr ].
keyValue = 209 ifTrue: [ Transcript show: ''you pressed the Ñ key '' ; cr ].
keyValue = 210 ifTrue: [ Transcript show: ''you pressed the Ò key '' ; cr ].
keyValue = 211 ifTrue: [ Transcript show: ''you pressed the Ó key '' ; cr ].
keyValue = 212 ifTrue: [ Transcript show: ''you pressed the Ô key '' ; cr ].
keyValue = 213 ifTrue: [ Transcript show: ''you pressed the Õ key '' ; cr ].
keyValue = 214 ifTrue: [ Transcript show: ''you pressed the Ö key '' ; cr ].
keyValue = 215 ifTrue: [ Transcript show: ''you pressed the × key '' ; cr ].
keyValue = 216 ifTrue: [ Transcript show: ''you pressed the Ø key '' ; cr ].
keyValue = 217 ifTrue: [ Transcript show: ''you pressed the Ù key '' ; cr ].
keyValue = 218 ifTrue: [ Transcript show: ''you pressed the Ú key '' ; cr ].
keyValue = 219 ifTrue: [ Transcript show: ''you pressed the Û key '' ; cr ].
keyValue = 220 ifTrue: [ Transcript show: ''you pressed the Ü key '' ; cr ].
keyValue = 221 ifTrue: [ Transcript show: ''you pressed the Ý key '' ; cr ].
keyValue = 222 ifTrue: [ Transcript show: ''you pressed the Þ key '' ; cr ].
keyValue = 223 ifTrue: [ Transcript show: ''you pressed the ß key '' ; cr ].
keyValue = 224 ifTrue: [ Transcript show: ''you pressed the à key '' ; cr ].
keyValue = 225 ifTrue: [ Transcript show: ''you pressed the á key '' ; cr ].
keyValue = 226 ifTrue: [ Transcript show: ''you pressed the â key '' ; cr ].
keyValue = 227 ifTrue: [ Transcript show: ''you pressed the ã key '' ; cr ].
keyValue = 228 ifTrue: [ Transcript show: ''you pressed the ä key '' ; cr ].
keyValue = 229 ifTrue: [ Transcript show: ''you pressed the å key '' ; cr ].
keyValue = 230 ifTrue: [ Transcript show: ''you pressed the æ key '' ; cr ].
keyValue = 231 ifTrue: [ Transcript show: ''you pressed the ç key '' ; cr ].
keyValue = 232 ifTrue: [ Transcript show: ''you pressed the è key '' ; cr ].
keyValue = 233 ifTrue: [ Transcript show: ''you pressed the é key '' ; cr ].
keyValue = 234 ifTrue: [ Transcript show: ''you pressed the ê key '' ; cr ].
keyValue = 235 ifTrue: [ Transcript show: ''you pressed the ë key '' ; cr ].
keyValue = 236 ifTrue: [ Transcript show: ''you pressed the ì key '' ; cr ].
keyValue = 237 ifTrue: [ Transcript show: ''you pressed the í key '' ; cr ].
keyValue = 238 ifTrue: [ Transcript show: ''you pressed the î key '' ; cr ].
keyValue = 239 ifTrue: [ Transcript show: ''you pressed the ï key '' ; cr ].
keyValue = 240 ifTrue: [ Transcript show: ''you pressed the ð key '' ; cr ].
keyValue = 241 ifTrue: [ Transcript show: ''you pressed the ñ key '' ; cr ].
keyValue = 242 ifTrue: [ Transcript show: ''you pressed the ò key '' ; cr ].
keyValue = 243 ifTrue: [ Transcript show: ''you pressed the ó key '' ; cr ].
keyValue = 244 ifTrue: [ Transcript show: ''you pressed the ô key '' ; cr ].
keyValue = 245 ifTrue: [ Transcript show: ''you pressed the õ key '' ; cr ].
keyValue = 246 ifTrue: [ Transcript show: ''you pressed the ö key '' ; cr ].
keyValue = 247 ifTrue: [ Transcript show: ''you pressed the ÷ key '' ; cr ].
keyValue = 248 ifTrue: [ Transcript show: ''you pressed the ø key '' ; cr ].
keyValue = 249 ifTrue: [ Transcript show: ''you pressed the ù key '' ; cr ].
keyValue = 250 ifTrue: [ Transcript show: ''you pressed the ú key '' ; cr ].
keyValue = 251 ifTrue: [ Transcript show: ''you pressed the û key '' ; cr ].
keyValue = 252 ifTrue: [ Transcript show: ''you pressed the ü key '' ; cr ].
keyValue = 253 ifTrue: [ Transcript show: ''you pressed the ý key '' ; cr ].
keyValue = 254 ifTrue: [ Transcript show: ''you pressed the þ key '' ; cr ].
keyValue = 255 ifTrue: [ Transcript show: ''you pressed the ÿ key '' ; cr ].
     Transcript show: ''you pressed the '' ; show:  keyValue ;  cr.
  self changed '
classified: 'interaction'.


Sokoban compile: 'handlesMouseOver: anEvent  
  ^ true.'
classified: 'actions'.


Sokoban compile: 'initialize  	
 super initialize.
 self color: Color blue .  
 self position: 300@300 . 
 self extent: 600@600.
 self level1 .  
 self openInWorld.'
classified: 'startup'.



" gives us keyboard focus when mouse pointer is over the sokoban itself "
" restart animation when focus regained as does not do anything with keyboard focus"

Sokoban compile: 'mouseEnter: anEvent 
   anEvent hand newKeyboardFocus: self.
   self startStepping.
   Transcript show: ''mouse enter'' ; cr . '
	classified: 'mouse-focus'.


Sokoban compile: 'mouseLeave: anEvent 
    anEvent hand newKeyboardFocus: nil.
    self stopStepping.
    Transcript show: ''mouse leave'' ; cr . '
	classified: 'mouse-focus'.


#( got sufficent control to have an area on screen where stuff can happen 
 lets get back to solving sokoban - do we have a model in mind of how the game will 
 work 
 some sort of level , if get into trouble , can reset game , or skip the level
								
).



"self startStepping."
"self stopStepping."

Sokoban compile: 'wantsSteps
  ^ true'
classified: 'animation'.

Sokoban compile: 'stepTime
 "milliseconds"
  ^ 100'
classified: 'animation'.

Sokoban compile: 'step
  World doOneCycle.
  ^ true'
classified: 'animation'.

Sokoban compile: 'board  
  ^ board'
classified: 'model'.


" === === " 
"$_ empty"
"$x wall"
"$a player"
"$b box"
" === can cope with being moved on screen  === "
Sokoban compile: 'drawOn: aCanvas
 | val c ownerPos px py px2 py2 origin corner cellWidth cellHeight |
 width isNil ifTrue:[ ^false]. 
 height isNil ifTrue: [ ^false].  
 cellWidth := 50 . 
 cellHeight := 50 .
 ownerPos := self position . 
 "self position: 0@0 ." 
 self extent: ((width + 3) * cellWidth) @ ((height + 3) * cellHeight) .
 aCanvas fillRectangle: (Rectangle origin: ownerPos corner: ownerPos + (((width+2) * cellWidth) @ ((height +2) * cellHeight))) color: Color black. 
 1 to: width do: [ :x |
  1 to: height do: [ :y |
	   "scale"
	   px := (x * cellWidth)+ 5. 
	   py := (y * cellWidth) + 5. 
	   px2 := (px + cellWidth - 5) . 
	   py2 := (py + cellWidth - 5) . 
		"adjust"
		""
	   origin := ownerPos + (px @ py) . 
	   corner := ownerPos + (px2 @ py2) . 
      val := board at: (x@y).
      val = $_ ifTrue: [ c := Color green ].
      val = $a ifTrue: [ c := Color blue].
      val = $x ifTrue: [ c := Color gray].
      val = $b ifTrue: [ c := Color orange].
      aCanvas fillRectangle: (Rectangle origin: origin corner: corner) 
              color: c ]].'
	classified: 'drawing'.

" === how do we cope with resizing === "
" === what is a Form  ==="
" === SokobanCell ==="
" === SokobanWallCell - acts as a wall ==="
" === SokobanTargetCell - acts as an empty cell but displays itself differently ==="
" === SokobanEmptyCell - acts as an empty cell ==="

#(
x x x x x 
x a _ _ x 
x _ b _ x 
x _ _ _ x 
x x x x x 
).
Sokoban compile: 'level1
width := 5 . 
height := 5 . 
board := Dictionary new . 
  1 to: width do: [:x |   	1 to: height do: [:y |  board at: (x@y) put: $_ ]] .
board at:(1@1) put: $x .
board at:(2@1) put: $x .
board at:(3@1) put: $x .
board at:(4@1) put: $x .
board at:(5@1) put: $x .
board at:(1@2) put: $x .
player := (2@2) .
board at:(2@2) put: $a .
board at:(3@2) put: $_ .
board at:(4@2) put: $_ .
board at:(5@2) put: $x .
board at:(1@3) put: $x .
board at:(2@3) put: $_ .
board at:(3@3) put: $b .
board at:(4@3) put: $_ .
board at:(5@3) put: $x .
board at:(1@4) put: $x .
board at:(2@4) put: $_ .
board at:(3@4) put: $_ .
board at:(4@4) put: $_ .
board at:(5@4) put: $x .
board at:(1@5) put: $x .
board at:(2@5) put: $x .
board at:(3@5) put: $x .
board at:(4@5) put: $x .
board at:(5@5) put: $x .
 self changed . ' classified: 'levels' . 

#(
x x x x x x x x 
x a _ x c _ _ x 
x _ b x x _ _ x 
x _ _ _ _ _ _ x 
x _ _ x _ x _ x 
x x x x x x x x 
).
Sokoban compile: 'level2
width := 8 . 
height := 6 . 
board := Dictionary new . 
  1 to: width do: [:x |   	1 to: height do: [:y |  board at: (x@y) put: $_ ]] .
board at:(1@1) put: $x .
board at:(2@1) put: $x .
board at:(3@1) put: $x .
board at:(4@1) put: $x .
board at:(5@1) put: $x .
board at:(6@1) put: $x .
board at:(7@1) put: $x .
board at:(8@1) put: $x .
board at:(1@2) put: $x .
player := (2@2) .
board at:(2@2) put: $a .
board at:(3@2) put: $_ .
board at:(4@2) put: $x .
board at:(5@2) put: $c .
board at:(6@2) put: $_ .
board at:(7@2) put: $_ .
board at:(8@2) put: $x .
board at:(1@3) put: $x .
board at:(2@3) put: $_ .
board at:(3@3) put: $b .
board at:(4@3) put: $x .
board at:(5@3) put: $x .
board at:(6@3) put: $_ .
board at:(7@3) put: $_ .
board at:(8@3) put: $x .
board at:(1@4) put: $x .
board at:(2@4) put: $_ .
board at:(3@4) put: $_ .
board at:(4@4) put: $_ .
board at:(5@4) put: $_ .
board at:(6@4) put: $_ .
board at:(7@4) put: $_ .
board at:(8@4) put: $x .
board at:(1@5) put: $x .
board at:(2@5) put: $_ .
board at:(3@5) put: $_ .
board at:(4@5) put: $x .
board at:(5@5) put: $_ .
board at:(6@5) put: $x .
board at:(7@5) put: $_ .
board at:(8@5) put: $x .
board at:(1@6) put: $x .
board at:(2@6) put: $x .
board at:(3@6) put: $x .
board at:(4@6) put: $x .
board at:(5@6) put: $x .
board at:(6@6) put: $x .
board at:(7@6) put: $x .
board at:(8@6) put: $x .
 self changed . ' classified: 'levels' . 

#(
x x x x x x x x x x 
x a _ x _ _ _ x _ x 
x _ b x x x _ _ _ x 
x _ _ x x x _ _ x x 
x _ _ _ _ _ _ _ _ x 
x _ _ x _ _ x _ _ x 
x x x x x x x x x x 
).
Sokoban compile: 'level3
width := 10 . 
height := 7 . 
board := Dictionary new . 
  1 to: width do: [:x |   	1 to: height do: [:y |  board at: (x@y) put: $_ ]] .
board at:(1@1) put: $x .
board at:(2@1) put: $x .
board at:(3@1) put: $x .
board at:(4@1) put: $x .
board at:(5@1) put: $x .
board at:(6@1) put: $x .
board at:(7@1) put: $x .
board at:(8@1) put: $x .
board at:(9@1) put: $x .
board at:(10@1) put: $x .
board at:(1@2) put: $x .
player := (2@2) .
board at:(2@2) put: $a .
board at:(3@2) put: $_ .
board at:(4@2) put: $x .
board at:(5@2) put: $_ .
board at:(6@2) put: $_ .
board at:(7@2) put: $_ .
board at:(8@2) put: $x .
board at:(9@2) put: $_ .
board at:(10@2) put: $x .
board at:(1@3) put: $x .
board at:(2@3) put: $_ .
board at:(3@3) put: $b .
board at:(4@3) put: $x .
board at:(5@3) put: $x .
board at:(6@3) put: $x .
board at:(7@3) put: $_ .
board at:(8@3) put: $_ .
board at:(9@3) put: $_ .
board at:(10@3) put: $x .
board at:(1@4) put: $x .
board at:(2@4) put: $_ .
board at:(3@4) put: $_ .
board at:(4@4) put: $x .
board at:(5@4) put: $x .
board at:(6@4) put: $x .
board at:(7@4) put: $_ .
board at:(8@4) put: $_ .
board at:(9@4) put: $x .
board at:(10@4) put: $x .
board at:(1@5) put: $x .
board at:(2@5) put: $_ .
board at:(3@5) put: $_ .
board at:(4@5) put: $_ .
board at:(5@5) put: $_ .
board at:(6@5) put: $_ .
board at:(7@5) put: $_ .
board at:(8@5) put: $_ .
board at:(9@5) put: $_ .
board at:(10@5) put: $x .
board at:(1@6) put: $x .
board at:(2@6) put: $_ .
board at:(3@6) put: $_ .
board at:(4@6) put: $x .
board at:(5@6) put: $_ .
board at:(6@6) put: $_ .
board at:(7@6) put: $x .
board at:(8@6) put: $_ .
board at:(9@6) put: $_ .
board at:(10@6) put: $x .
board at:(1@7) put: $x .
board at:(2@7) put: $x .
board at:(3@7) put: $x .
board at:(4@7) put: $x .
board at:(5@7) put: $x .
board at:(6@7) put: $x .
board at:(7@7) put: $x .
board at:(8@7) put: $x .
board at:(9@7) put: $x .
board at:(10@7) put: $x .
 self changed . ' classified: 'levels' . 

#(
x x x x x x x x x x x x x 
x a _ x _ _ _ _ _ _ x _ x 
x _ b x _ b _ x x _ _ _ x 
x _ _ x _ _ x x x b _ x x 
x x _ _ b x x _ x _ _ _ x 
x x _ _ _ _ x _ b _ _ x x 
x x x _ _ _ b _ _ _ x x x 
x x x x _ _ _ _ _ x x x x 
x x x x x x x x x x x x x 
).
Sokoban compile: 'level4
width := 13 . 
height := 9 . 
board := Dictionary new . 
  1 to: width do: [:x |   	1 to: height do: [:y |  board at: (x@y) put: $_ ]] .
board at:(1@1) put: $x .
board at:(2@1) put: $x .
board at:(3@1) put: $x .
board at:(4@1) put: $x .
board at:(5@1) put: $x .
board at:(6@1) put: $x .
board at:(7@1) put: $x .
board at:(8@1) put: $x .
board at:(9@1) put: $x .
board at:(10@1) put: $x .
board at:(11@1) put: $x .
board at:(12@1) put: $x .
board at:(13@1) put: $x .
board at:(1@2) put: $x .
player := (2@2) .
board at:(2@2) put: $a .
board at:(3@2) put: $_ .
board at:(4@2) put: $x .
board at:(5@2) put: $_ .
board at:(6@2) put: $_ .
board at:(7@2) put: $_ .
board at:(8@2) put: $_ .
board at:(9@2) put: $_ .
board at:(10@2) put: $_ .
board at:(11@2) put: $x .
board at:(12@2) put: $_ .
board at:(13@2) put: $x .
board at:(1@3) put: $x .
board at:(2@3) put: $_ .
board at:(3@3) put: $b .
board at:(4@3) put: $x .
board at:(5@3) put: $_ .
board at:(6@3) put: $b .
board at:(7@3) put: $_ .
board at:(8@3) put: $x .
board at:(9@3) put: $x .
board at:(10@3) put: $_ .
board at:(11@3) put: $_ .
board at:(12@3) put: $_ .
board at:(13@3) put: $x .
board at:(1@4) put: $x .
board at:(2@4) put: $_ .
board at:(3@4) put: $_ .
board at:(4@4) put: $x .
board at:(5@4) put: $_ .
board at:(6@4) put: $_ .
board at:(7@4) put: $x .
board at:(8@4) put: $x .
board at:(9@4) put: $x .
board at:(10@4) put: $b .
board at:(11@4) put: $_ .
board at:(12@4) put: $x .
board at:(13@4) put: $x .
board at:(1@5) put: $x .
board at:(2@5) put: $x .
board at:(3@5) put: $_ .
board at:(4@5) put: $_ .
board at:(5@5) put: $b .
board at:(6@5) put: $x .
board at:(7@5) put: $x .
board at:(8@5) put: $_ .
board at:(9@5) put: $x .
board at:(10@5) put: $_ .
board at:(11@5) put: $_ .
board at:(12@5) put: $_ .
board at:(13@5) put: $x .
board at:(1@6) put: $x .
board at:(2@6) put: $x .
board at:(3@6) put: $_ .
board at:(4@6) put: $_ .
board at:(5@6) put: $_ .
board at:(6@6) put: $_ .
board at:(7@6) put: $x .
board at:(8@6) put: $_ .
board at:(9@6) put: $b .
board at:(10@6) put: $_ .
board at:(11@6) put: $_ .
board at:(12@6) put: $x .
board at:(13@6) put: $x .
board at:(1@7) put: $x .
board at:(2@7) put: $x .
board at:(3@7) put: $x .
board at:(4@7) put: $_ .
board at:(5@7) put: $_ .
board at:(6@7) put: $_ .
board at:(7@7) put: $b .
board at:(8@7) put: $_ .
board at:(9@7) put: $_ .
board at:(10@7) put: $_ .
board at:(11@7) put: $x .
board at:(12@7) put: $x .
board at:(13@7) put: $x .
board at:(1@8) put: $x .
board at:(2@8) put: $x .
board at:(3@8) put: $x .
board at:(4@8) put: $x .
board at:(5@8) put: $_ .
board at:(6@8) put: $_ .
board at:(7@8) put: $_ .
board at:(8@8) put: $_ .
board at:(9@8) put: $_ .
board at:(10@8) put: $x .
board at:(11@8) put: $x .
board at:(12@8) put: $x .
board at:(13@8) put: $x .
board at:(1@9) put: $x .
board at:(2@9) put: $x .
board at:(3@9) put: $x .
board at:(4@9) put: $x .
board at:(5@9) put: $x .
board at:(6@9) put: $x .
board at:(7@9) put: $x .
board at:(8@9) put: $x .
board at:(9@9) put: $x .
board at:(10@9) put: $x .
board at:(11@9) put: $x .
board at:(12@9) put: $x .
board at:(13@9) put: $x .
 self changed . ' classified: 'levels' .











