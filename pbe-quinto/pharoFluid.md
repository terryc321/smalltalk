# Pharo 13 Fluid Syntax 

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

the old method will definitely NOT work.

