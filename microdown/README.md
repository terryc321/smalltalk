
# Microdown 

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

