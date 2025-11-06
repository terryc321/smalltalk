
# explore implementation of String class in smalltalk 

# non uniforn 

cannot just say browse on any object and have inspector open 

think possibly inspect message 

cuis smalltalk 

difficulty even browsing String class 

in cuis have to check various types of String unicode Text class - is there documentation 

trying to look at the implementation of cuis string class 

String new ; browse.

'this is something to browse.' browse.


# hierachy 

are these classes or packages ?

Object -> Collection -> SequenceableCollection -> CharacterSequence -> String -> Symbol 

Kernel package -> Objects -> 
package : Kernel-Objects 
class : Object 

Object 
mouse over select & #browseIt

Object new class windowColor.  (Color r: 0.768 g: 0.768 b: 0.768) .
Smalltalk class windowColor.  (Color r: 0.768 g: 0.768 b: 0.768) .

A randomw object has no package 

| o |
o := Object new. 
o class package . nil . 

| o |
o := Object new. 
o class whatIsAPrimitive . 

whatIsAPrimitive
^ self error: 'comment only'

self 

yourself 

self subclassResponsibility.

self shouldNotImplement.

'Fred
the
Bear' lineCount


'dec' asMonth .
'wisper' asNFC . 
'wisper' asNFD . 

'1234567890' asNumber . 

'123123123' asNumber.  123123123 .
'-2' asNumber.  -2 .
'3-2i' asNumber.  -2 .
'3-2i' asNumber.   3 .

'asdf' asCodePoints. 
'asdf' size . 


Language agnostic software development design

python
php
smalltalk OOP language
clojure
self 


