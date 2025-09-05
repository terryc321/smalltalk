# PBE-quinto

pharo by example

tried in glamorous toolkit - like no Worlds , forced into this web interface like notebook

```
SimpleSwitchMorph new openInWorld.
SimpleSwitchMorph new openInHand.
```

## Pharo 13 Fluid Syntax 

A new way to define a class

```
Object subclass: #Car
    instanceVariableNames: 'make model year'
    classVariableNames: ''
    package: 'Garage'
```	

becomes

```
Object << #Car
    slot: { #make .  #model .  #year } ; 
    package: 'Garage'
```

## 


This 

```
```

```
c := LOCell new openInWorld. 
c position: 500@500 . 
c extent: 16@16 . 
c turnOff.
c turnOn. 


b := LOGame new openInWorld. 
b position: 50@50 . 
b extent: (10*16)@(10*16) . 
b inspect. 


c slotNamed: #cells. 

" No matrix in pharo 13"
Matrix m := ?? Matrix undefined

```
