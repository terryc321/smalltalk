# quinto

proved it is possible to solve did this by hand

did not record steps taken

idea is to start in lower right corner, constantly try create an ever increasing space of ON blue buttons , 
eventually can create a grid of 

what is the minimal number of steps required to solve the 10 x 10 grid ?

easier way is conjecture it takes X steps 

then refute conjecture by providing a proof in form of counter-example

* chicken scheme *

stuck on 2 d array 
#a2r((#f #f #f)
     (#f #f #f)
	 (#f #f #f))
	 
	 

```
huge casm wasteland to understand efficient model checking

first order logic

a light switch is either on or off , for this we have a predicate something that is either true or false 

predicate On(x,y,t) 

meaning that the light at x y is either On or Off at time t 
x int x >= 0 and x <= 10 
y int y >= 0 and y <= 10
t int t >= 0



not On(1,1,0).
not On(1,2,0).
not On(1,3,0).
not On(1,4,0).
...
not On(2,1,0).
not On(2,2,0).
not On(2,3,0).
not On(2,4,0).
..
not On(10,10,0).


only one switch can be switch on at a time - we want to know a sequence of switch activations 

;; we can be brutally specific saying activate 1 1 at time t
;; we could go further and specify for each individual time step 
forall t Activate(1,1,t) then { On(1,2,t) -> not On(1,2,t + 1)}

Toggle(x,y,t) -> 


s_01_01 is a boolean 

state transition
state (s_01_01 , s_01_02 , s_01_03 , s_01_04 ... s_10_10) 

transition state -> state

state (0,0,0,0,0, ...) 


```



