#lang racket

(provide (all-defined-out))

(require "automata.rkt")

;; CONFIGURATION
(define MAX-STATES# 3) ; create an automaton having up to 15 states
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
      (interact auto1 auto2 rounds delta))
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


(define (regenerate population rate)
  (define probabilities (payoff->fitness population))
  [define substitutes (choose-randomly probabilities rate)]
  (for ([i (in-range rate)]
        [auto (in-list substitutes)])
    (vector-set! population i (clone
