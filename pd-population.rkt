#lang racket

(provide (all-defined-out))

(require "pd-automata.rkt" "inout.rkt")

;; CONFIGURATION
(define MAX-STATES# 1) ; create an automaton having up to 15 states
;; even start with 1

;; POPULATION
;; can we do it immutable?
(define (build-random-population n)
  (build-vector n (lambda (_) (make-random-automaton MAX-STATES#))))

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
    (vector-set! population (+ i 1) a2)
(out-data "data" (list (list round-results a1 a2)))
)
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


(define (evolve population cycles speed rounds delta mutation)
  (cond
   [(zero? cycles) '()]
   [else (out-data "data" (list (list cycles)))
         (define p2 (match-population population rounds delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (define p4 (mutate-population p3 mutation))
         (cons (average pp)
               ;p3
               (evolve p3 (- cycles 1) speed rounds delta mutation))]))

(require plot)
(define (population-mean->lines data)
  (define coors (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))
  (lines coors))

(define (plot-mean data)
(plot (list (population-mean->lines data)) #:out-file "trial.png"
 #:y-max 5 #:y-min 0))

(define (main)
(collect-garbage)
(define A (build-random-population 100))
(define data (time (evolve A 800 10 100 .9 1)))
(plot-mean data))

(module+ five
  (main)
  (main)
  (main)
  (main)
  (main))


