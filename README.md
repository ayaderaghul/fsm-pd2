# fsm-pd2

## rationale
so vincent emailed me

and i started again the simulation, under his advision

some notable features are:

* the data structure of automata is represented in hash table
* immutable mutation
* in interaction, delta is the continual probability
* redo the population (previous: double vectored population)
* mutation: add and delete states
* try to minimize matha in the work flow (export automata code, solve games w matrix form, plot replicator dynamics etc)

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

1. 7 sep: work flow to develop the benchmark automata in the personality test

ressurect at

then find a way to test their personality

then collect the useful benchmarks into a table

that would be materials for selecting an official benchmark list

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
|12|in each cycle, scan -> export dot codes together with statistics|x|5||produce multiple graphs in .ps then pdf (dot -Tps2 ... | ps2pdf ...)|
|13|bc i have to export automata into txt then recover -> it's better (for the later reading) to flatten them first|x|5|||
|14|export: odd line - cycle, even line - all the rankings in one line, if null leave an empty line|x|6||easier for import|
|15|now import & recover automata|x|7|||
|16|ressurect-at|x|8|||
||test personality at cycle 2000, sample|||||
||get matha out of the loop|||||
||revise code, in light of these new discoveries haha: list-update, list-set, [j (in-naturals), [e l] ~ [e (in-list l)]]|||||
||write br function, solve NE|x|9|| |
||run semi extensive simulations to get patterns, in order to develop benchmark set for the personality test|||| |
||export graphviz to pdf: big automata -> dont fit into A4 page of pdf, but postscript file is ok|||| |
||generate file names reflecting different deltas|||| |
||run across deltas|x|9|| |
||you dont have time for developing the personality test. just see what happens in the special periods|||| :((( |
||generate more data to see the patterns as delta runs along the spectrum (middle high deltas: more interesting, delta close to 1: mostly tit for tat)|||| |
|||||| |
|||||| |
|||||| |
|||||| |





