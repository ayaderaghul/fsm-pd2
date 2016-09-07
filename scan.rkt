#lang racket
(require "pd-automata.rkt")
(provide (all-defined-out))

(define (scan population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   p))

;; flatten
(define (flatten-automaton au)
  (match-define (automaton head body) au)
  (flatten
   (list
    (hash-ref head 'INITIAL)
    (for/list ([(key value) (in-hash body)])
      (match-define (state action dispatch) value)
      (list
       key
       (list
        (if (equal? action 'D) 1 0)
        (for/list ([(key value) (in-hash dispatch)])
          value)))))))

(define STATE-LENGTH (+ 2 ACTIONS#))

(define (recover-automaton au)
  (match-define (list* init body) au)
  (define head (hash 'INITIAL init 'PAYOFF 0 'CURRENT init))
  (define s (/ (length body) STATE-LENGTH))
  (define (recover-body s a-body)
    (cond [(empty? a-body) '()]
          [else
           (define-values (first-state the-rest)
             (split-at a-body STATE-LENGTH))
           (match-define (list state-id label d? c?)
                         first-state)
           (append (list
                    state-id
                    (state
                     (if (zero? label) 'C 'D)
                     (hash 'D d? 'C c?)))
                   (recover-body s the-rest))]))
  (automaton head (apply hash (recover-body s body))))





(define (scan-flatten population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h (flatten-automaton au) add1 0))
   (hash)
   p))

(define (hash-ref* a-hash a-key)
  (if (hash-has-key? a-hash a-key)
      (hash-ref a-hash a-key)
      0))


(define (scan-defects-tft population)
  (let ([ranking (scan (vector-map reset population))]
        [d (defects)]
        [t (tit-for-tat)])
    (list
     (hash-ref* ranking d)
     (hash-ref* ranking t))))
