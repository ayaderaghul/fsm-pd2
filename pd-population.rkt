#lang racket

(provide (all-defined-out))
(require "pd-automata.rkt" "pd-scan.rkt" "pd-inout.rkt" plot)

;; CONFIGURATION
(define N 100)
(define CYCLES 5000)
(define SPEED 10)
(define ROUNDS 400)
(define DELTA .95)
(define MUTATION 10)


;; POPULATION
;; can we do it immutable?
(define (build-random-population n)
  (build-vector n (lambda (_) (make-random-automaton 1))))

(define (population-payoffs population)
  (define (retrieve-payoff automaton)
    (hash-ref (automaton-head automaton) 'PAYOFF))
  (for/list ([auto population]) (retrieve-payoff auto)))

(define (match-population population rounds delta)
  (population-reset population)
  (for ([i (in-range 0 (- (vector-length population) 1) 2)])
    (define auto1 (vector-ref population i))
    (define auto2 (vector-ref population (+ i 1)))
    (define-values (round-results a1 a2)
      (interact* auto1 auto2 rounds delta))
    (vector-set! population i a1)
    (vector-set! population (+ i 1) a2))
  population)

(define (population-reset population)
  (for ([auto population] [i (in-naturals)])
    (vector-set! population i (reset auto))))

(define (sum l)
  (apply + l))

(define (payoff->fitness population)
  (define payoffs (population-payoffs population))
  (define total (sum payoffs))
  (for/list ([p (in-list payoffs)])
    (/ p total)))

(define (accumulate-fitness probabilities)
  (let relative->absolute ([payoffs probabilities][so-far #i0.0])
    (cond
     [(empty? payoffs) '()]
     [else (define nxt (+ so-far (first payoffs)))
           (cons nxt (relative->absolute (rest payoffs) nxt))])))

(define (randomise probabilities speed #:random (q #false))
  (define fitness (accumulate-fitness probabilities))
  (for/list ([n (in-range speed)])
    [define r (or q (random))]
    ;; population is non-empty so there will be some i such that ...
    (for/last ([p (in-naturals)] [% (in-list fitness)] #:final (< r %)) p)))

(define (regenerate population rate)
  (define probabilities (payoff->fitness population))
  [define substitutes (randomise probabilities rate)]
  (for ([i (in-range rate)]
        [auto (in-list substitutes)])
    (vector-set! population i (reset (vector-ref population auto))))
  (shuffle-vector population))

;; MUTATE
(define (mutate-population population rate)
  (for ([i (in-range rate)])
    (vector-set! population i (mutate (vector-ref population i)))))

;; SHUFFLE VECTOR

(define (shuffle-vector vec)
  (define lst (vector->list vec))
  (define l2 (shuffle lst))
  (list->vector l2))

(define (average lst)
  (exact->inexact
   (/ (sum lst)
     (length lst))))

(define (evolve population cycles speed rounds delta mutation rank-file)
  (cond
   [(zero? cycles) '()]
   [else
    (define rankings (scan-flatten (vector-map reset population)))
    (out-rank cycles rankings rank-file)
    (define p2 (match-population population rounds delta))
    (define pp (population-payoffs p2))
    (define p3 (regenerate p2 speed))
    (define p4 (mutate-population p3 mutation))
    (cons (average pp)
                                        ;p3
          (evolve p3 (- cycles 1) speed rounds delta mutation rank-file))]))

(define (population-mean->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

(define (plot-mean data delta rounds pic-file pic-name)
  ;(define reward (* 3 (compound delta rounds)))
  ;(define punishment (* 1 (compound delta rounds)))
  (define reward 3)
(define punishment 1)
  (define reward-line (function (lambda (x) reward) #:color "blue"))
  (define punishment-line (function (lambda (x) punishment) #:color "red"))
  (plot (list reward-line punishment-line
              (population-mean->lines data))
        #:x-label "cycles" #:y-label "population mean"
        #:out-file pic-file #:title pic-name
        #:y-max (+ reward 5) #:y-min 0 #:width 1200 #:height 800))

;; to calculate the compound rate of payoff
(define (compound d r)
  (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))


;; GENERATE NAMES AND TITLES

(define (variable->string x)
  (string-trim (number->string (* 100 x)) ".0"))
(define (generate-file-name x suffix)
  (string-append (variable->string x) suffix))

(define (main)
 (collect-garbage)
 (define pic-title (configuration-string N SPEED ROUNDS DELTA))
 (define pic-file (generate-file-name DELTA "pic.png"))
 (define rank-file (generate-file-name DELTA "rank"))
 (define mean-file (generate-file-name DELTA "mean"))
 (define A (build-random-population N))
 (define data (time (evolve A CYCLES SPEED ROUNDS DELTA MUTATION rank-file)))
 (out-mean mean-file data)
 (plot-mean data DELTA ROUNDS pic-file pic-title))

(module+ five
  (main)
  (main)
  (main)
  (main)
  (main))
