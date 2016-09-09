#lang racket

(require "pd-population.rkt" "inout.rkt")
(provide (all-defined-out))
(define DELTAS (list 0 .2 .5 .8 .9 .95 .99))

(define (deltas)
  (for ([i DELTAS])
    (collect-garbage)
    (define pic-title (configuration-string N SPEED ROUNDS i))
    (define pic-file (generate-file-name i "pic.png"))
    (define rank-file (generate-file-name i "rank"))
    (define mean-file (generate-file-name i "mean"))
    (define A (build-random-population N))
    (define data (time (evolve A CYCLES SPEED ROUNDS i MUTATION rank-file)))
    (out-mean mean-file data)
    (plot-mean data i ROUNDS pic-file pic-title)))

(module+ main (deltas))
