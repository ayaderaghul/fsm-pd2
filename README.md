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

| ID| To do          | Done?|Date    | by | Note |
|:-:|-------------|:-----:|:-----:|:-----:|----|
| 1 | redo the population    | x| 0309| chi |works ..ok (shows the cooperative periods) |
| 2 | evolve doesnt work: hash-ref value not found: interaction |x |3108 ||blame: mutation, because the exception is thrown at arbitrary settings|
| 3 | export to graphviz dot instead of matha code |x | 24 ||with-output-to-dot|
| 4 | reset: both current and payoff  |x| 23 |||
| 5 | visualise the mutation process on TV  | |||ugh later..|
| 6 | why it doesnt show cooperative period |x|0109 ||try to test the rd first, wo mutation: fail. blame: `scan right, but forget to reset payoff before scan|
| 7 | replicator dynamics test looks squiggly ugly | ||| blame: continual probability method. change to discount method, still ugly |
| 8 | from #6, if rd test passed, why it doesnt show cooperative period? |x|2||STILL DOESNT SHOW COOPERATIVE PERIOD??? WTF. finally it shows, but with very high mutation rate|
|9|the characteristic test|||||
|10|see how to export w printf in the link vincent sent|o|||with printf, you dont have to string-append. just print, and with-output-to-file will collect them into output file. however, i print different parts with `for and plug them together. so.. task aborted.|
|11|from #5: show on tv the ranking then print them out over cycles||||pr'bly a combination of plot/dc or draw w graphviz, #:exist 'append|
|12|in each cycle, scan -> export dot codes together with statistics||||dont know how to produce multiple graphs in one png yet =.=|
|13||||||


