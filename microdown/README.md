
# Microdown 

also known as pillar 

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
