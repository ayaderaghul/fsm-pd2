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


2. holy god, at 31 aug i fix the bug #2

error:

    hash-ref: no 'D key
    blame: interact...

i have to output population at each cycle in the `evolve

then print "I-AM-HERE!!!!!" everytime it throws exception

then print output of `interact, with handlers, so if it throws exception, it still returns the automaton pair

i discover that the cause is that somehow the states in the troublesome automaton gets an extra pair of parentheses (OMG)

i guess it is due to `mutate

then print output of `mutate with its ID

then i discover that it is the `detach state that adds the extra pair

because hash->list and list->hash i have to add `flatten

## to-very-dos

| ID         | To do          | Date    | Done? | Note| by |
|-------------- |:-------------:| ----- |:-----:|-----|----|


| ID | To do         | Date          | By    | Done? | Note|
| -- | ------------- |:-------------:| ----- |:-----:|-----|


| 1 | redo the population    | 22 aug | chi | ||
| 2 | evolve doesnt work: hash-ref value not found: interaction  |31 ||x|blame: mutation, because the exception is thrown at arbitrary settings|
| 3 | export to graphviz dot instead of matha code  | 24 ||x|with-output-to-dot|
| 4 | reset: both current and payoff  | 23 ||x||
| 5 | visualise the mutation process on TV  | |||ugh later..|


