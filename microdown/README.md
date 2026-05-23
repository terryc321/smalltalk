
# Microdown 

## heres the working

"get a list of all classes held under NewTools 
my forked version called pharo-spec-NewTools
"

BaselineOfNewTools comment .

Smalltalk allClasses collect: #name 
(RPackageOrganizer default packageNamed: 'NewTools') definedClasses   
((Smalltalk image packages flatCollect: #classes) at: 1) className.
Smalltalk image packageAt: #NewTools ifAbsent: [ nil ]. 

Smalltalk image packages.
"lets select the packages beginning NewTools "
packages := (Smalltalk image packages) select: [:p | p name  beginsWith: 'NewTools-CodeCritiques'].
packages := (Smalltalk image packages) select: [:p | p name  beginsWith: 'NewTools'].
classes := packages flatCollect: [ :p | p classes ].

regex := '\[\[\[(.*)\]\]\]' asRegex.
classAndComment := classes collect:[ :c |
         | newComment | 
         newComment := regex copy: (c comment)
    translatingMatchesUsing: [ :match |
        '```' , (regex subexpression: 2) , '```' ].
		 { c . newComment } .
		 
		 




"eventually we got regex matching correctly "
regex := '\[\[\[(.*)\]\]\]' asRegex.
result := regex copy: '[[[ hello world aaa ]]]' 
    translatingMatchesUsing: [ :match |
        '```' , (regex subexpression: 2) , '```'
    ].


# regular expressions 

```
'hello' copyWithRegex: '[elo]+' matchesReplacedWith: 'i' 
'test [[[(hello)]]] end' copyWithRegex: '\[\[\[(.*)\]\]\]' matchesReplacedWith: '```{_1}```'.   
```

we can make a regex from a string but it doesnt get us anywhere 

```
"this does not work"
re := '\[\[\[(.*)\]\]\]' asRegex.
result := re replacingMatchesWith: '```{1}```'.
```

## NewTools

```
"get a list of all classes held under NewTools 
my forked version called pharo-spec-NewTools
"

BaselineOfNewTools comment .

Smalltalk allClasses collect: #name 
(RPackageOrganizer default packageNamed: 'NewTools') definedClasses   
((Smalltalk image packages flatCollect: #classes) at: 1) className.
Smalltalk image packageAt: #NewTools ifAbsent: [ nil ]. 

Smalltalk image packages.
"lets select the packages beginning NewTools "
packages := (Smalltalk image packages) select: [:p | p name  beginsWith: 'NewTools-CodeCritiques'].
packages := (Smalltalk image packages) select: [:p | p name  beginsWith: 'NewTools'].
classes := packages flatCollect: [ :p | p classes ].
classAndComment := classes collect:[ :c | {c . c comment} ].
(classAndComment select:[ :c | (c at: 2) isEmpty not ]) select: [:c | (c at: 2) .
```



## NewTools

Lets fix some documentation - lets select package NewTools and any sub packages 
lets for each class listed - 

```
"lets select the packages beginning NewTools "
(Smalltalk image packages) select: [:p | p name  beginsWith: 'NewTools'].
```

###

```
"get a list of all classes held under NewTools 
my forked version called pharo-spec-NewTools
"

BaselineOfNewTools comment .

Smalltalk allClasses collect: #name 
(RPackageOrganizer default packageNamed: 'NewTools') definedClasses   
((Smalltalk image packages flatCollect: #classes) at: 1) className.
Smalltalk image packageAt: #NewTools ifAbsent: [ nil ]. 

Smalltalk image packages.
"lets select the packages beginning NewTools "
(Smalltalk image packages) select: [:p | p name  beginsWith: 'NewTools'].

```

## 


```
root := Microdown new parseFile: ('/home/terry/code/NewPharoByExample9/Chapters/BasicClasses/BasicClasses.md' asFileReference).
```

## Pharo 13

playground

```
ref := (MicResourceReference fromUri: 'file:///home/terry/code/NewPharoByExample9/Chapters/BasicClasses/BasicClasses.md').
ref contents.
MicTextPresenter new
		  text: (Microdown asRichText: (ref contents) );
		  open
```

also known as pillar ? 

```
"we can view the index - it says cannot follow relative links"
ref := (MicResourceReference fromUri: 'file:///home/terry/code/NewPharoByExample9/index.md').
ref contents.

"we can view chapters of the book"
"we do not know which is first chapter - house numbered by name of house - fire fighter cannot find house"
ref := (MicResourceReference fromUri: 'file:///home/terry/code/NewPharoByExample9/Chapters/BasicClasses/BasicClasses.md').

ref := (MicResourceReference fromUri: 'file:///home/terry/code/NewPharoByExample9/Chapters/Collections/Collections.md').


MicTextPresenter new
		  text: (Microdown asRichText: (ref contents) );
		  open
```

but figures and images are not included in the output why why why 



# microdown browser presenter

```
StPresenter << #MicDocumentBrowserPresenter
	slots: {
			 #spButtonBar .
			 #spDocumentList .
			 #spRendering .
			 #spSource .
			 #saveButton .
			 #documentRoots .
			 #documentModel .
			 #layoutModel .
			 #disableEvents };
	sharedVariables: { #Browser };
	tag: 'GUI';
	package: 'NewTools-DocumentBrowser'
```

```
defaultDocumentRoots
	"By default include the pharo doc and the docs of loaded projects"
	^ ({'github://pharo-project/pharo/doc'},
		{'github://SquareBracketAssociates/NewPharoByExample9'},
	"{'github://SquareBracketAssociates/NewPharoByExample9/Chapters/BasicClasses/BasicClasses.md'},"
	"{'file://home/terry/code/NewPharoByExample9'},"
		self loadedDocsInWorkspace  )
			collect: #asMicResourceReference
```
		  
		  
can we view a microdown .md file from the Pharo By Example 9 github book ?

use spotter shift + enter and put microdown - look for either examples or tests - see a load uri test 

```
TestCase << #MicFileResourceReferenceTest
	slots: { #filesystem . #emptyFilesystem };
	tag: 'Resolution';
	package: 'Microdown-Tests'
```

can see loads from a file is a class based method on MicResourceReference

```
testLoadMicrodown
	| ref doc |
	ref := (MicResourceReference fromUri: 'file:/readme.md')
		fileSystem: filesystem.
	doc := ref loadMicrodown.
	self assert: doc children first class equals: MicHeaderBlock.
	self assert: doc children first text equals: 'Test documents' 	
```

