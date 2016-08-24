# fsm-pd2

## rationale
so vincent emailed me

and i started again the simulation, under his advision

some notable features are:

* the data structure of automata is represented in hash table
* immutable mutation
* in interaction, delta is the continual probability
* redo the population (previous: double vectored population)

## abstract
population of finite state machines playing the repeated prisoner's dilemma.
a sample code.

## log

1. only start to log at 22 august 2016

at this point, the state of the code is as follows:

automata.rkt
* basic automaton functions
* classic automata
* interaction (with delta as continual probability)
* immutable mutation (hash)



## to-very-dos

| To do         | Date          | By    | Done? | Note|
| ------------- |:-------------:| ----- |:-----:|-----|
| redo the population      | 22 aug 16 | chi | ||
| evolve doesnt work: hash-ref value not found: interaction  | |||blame: mutation|
| export to graphviz dot instead of matha code  | ||||working...|
| reset: both current and payoff  | 23 aug|||x||
|   | |||||
|   | |||||
|   | |||||
|   | |||||
|   | |||||



