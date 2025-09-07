


#( manually construct smalltalk package and classes required to play sokoban
we should be able to file in these files to various smalltalk programs that work with morphic 
slight changes will probably need to be made regarding naming of things 
we can simply have an interface class that delegates to various versions will systems accept code that 
).


Object subclass: #Volcano
       instanceVariableNames: ''
       classVariableNames: ''       
       package: 'SomeNew-Package'.

Volcano browse.

"lets now remove Volcano class entirely"

Smalltalk includesKey: #Volcano . "true"
Smalltalk removeClassNamed: #Volcano . 


"how go about designing a sokoban game - mind is so confused by object programming already "
"package PBE-Sokoban"
"SokobanGame becomes Sokoban inherits from BorderedMorph"

BorderedMorph subclass: #Sokoban
       instanceVariableNames: ''
       classVariableNames: ''       
       package: 'PBE-Sokoban'.



"how do add a class to sokoban "
"at the moment it is a cat"

Sokoban compile: 'makeSound
    "Make Cat object make sound."
    Transcript show: ''Meow!''.'
  classified: 'actions'.


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
keyValue = 27 ifTrue: [ Transcript show: ''you pressed the escape key '' ; cr ].
keyValue = 28 ifTrue: [ Transcript show: ''you pressed the left arrow   key '' ; cr ].
keyValue = 29 ifTrue: [ Transcript show: ''you pressed the right arrow   key '' ; cr ].
keyValue = 30 ifTrue: [ Transcript show: ''you pressed the up arrow key '' ; cr ].
keyValue = 31 ifTrue: [ Transcript show: ''you pressed the down arrow  key '' ; cr ].
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
keyValue = 97 ifTrue: [ Transcript show: ''you pressed the a key '' ; cr ].
keyValue = 98 ifTrue: [ Transcript show: ''you pressed the b key '' ; cr ].
keyValue = 99 ifTrue: [ Transcript show: ''you pressed the c key '' ; cr ].
keyValue = 100 ifTrue: [ Transcript show: ''you pressed the d key '' ; cr ].
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
keyValue = 115 ifTrue: [ Transcript show: ''you pressed the s key '' ; cr ].
keyValue = 116 ifTrue: [ Transcript show: ''you pressed the t key '' ; cr ].
keyValue = 117 ifTrue: [ Transcript show: ''you pressed the u key '' ; cr ].
keyValue = 118 ifTrue: [ Transcript show: ''you pressed the v key '' ; cr ].
keyValue = 119 ifTrue: [ Transcript show: ''you pressed the w key '' ; cr ].
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
     Transcript show: ''you pressed the '' ; show:  keyValue ;  cr. '    		     
    classified: 'interaction'.


Sokoban compile: 'handlesMouseOver: anEvent  
  ^ true.'
classified: 'actions'.


Sokoban compile: 'initialize  	
 super initialize.
 self color: Color black .  
 self position: 300@300 . 
 self extent: 600@600.  
 self openInWorld.'
classified: 'startup'.


" gives us keyboard focus when mouse pointer is over the sokoban itself " 

Sokoban compile: 'mouseEnter: anEvent 
   anEvent hand newKeyboardFocus: self.
   Transcript show: ''mouse enter'' ; cr . '
	classified: 'mouse-focus'.


Sokoban compile: 'mouseLeave: anEvent 
    anEvent hand newKeyboardFocus: nil.
    Transcript show: ''mouse leave'' ; cr . '
	classified: 'mouse-focus'.



Sokoban compile: 'threeMorphs
| m m2 m3 | 
m := Morph new openInWorld. 
m position: 100@100 . 
m extent: 50@50. 

m2 := Morph new openInWorld. 
m2 position: (m position) + ((m width) + 25@0).
m2 extent: 50@50. 

m3 := Morph new openInWorld. 
m3 position: (m2 position) + ((m2 width) + 25@0).
m3 extent: 50@50. '
classified: 'examples'


" ====================================================== "
		

" how represent a sokoban using a 2d grid  "
" how do we get a level - predesign level for a game from text file "
" how do read a file in smalltalk "
" how do we file out so we can read it back in from the playground - proper file out "










Sokoban compile: 'newBoxAt: x by: y  
 | morph | 
 morph := Morph new.
 morph setLocalPosXY: (x@y) .
 morph updateScreenPosition .
 "morph createImage: self. "
 ^ morph . ' .




self startStepping.
 self manualLevelOne.
 
]



