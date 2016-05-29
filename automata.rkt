#lang racket
(require racket/hash)
(provide (all-defined-out))

(define ACTIONS# 2)
(define ACTIONS (list 'C 'D))
(define (random-action)
  (list-ref ACTIONS (random ACTIONS#)))

(struct automaton (head body) #:transparent)
; body is a hash table of states
(struct state (action dispatch) #:transparent)


(define (make-random-automaton states#)
  (define initial-current (random states#))
  (define to-detach (random states#))
  (define (make-head) (hash 'INITIAL initial-current
                            'CURRENT initial-current
                            'PAYOFF 0))
  (define ids (build-list states# values))
  (define (make-body) (apply hash (flatten (map list ids (make-states)))))
  (define (make-states) (build-list states# make-state))
  (define (make-state _) (state (random-action) (make-transition)))
  (define (make-transition)
    (hash 'C (random states#)
          'D (random states#)))
  (automaton (make-head) (make-body)))

(define (reset a) ; reset
  (match-define (automaton head body) a)
  (define new-head
    (hash-set head 'CURRENT (hash-ref head 'INITIAL)))
  (automaton new-head body))

;; CLASSIC AUTOMATA
(define (cooperates)
  (define head (hash 'INITIAL 0 'CURRENT 0 'PAYOFF 0))
  (define body (hash 0 (state 'C (hash 'C 0 'D 0))))
  (automaton head body))

(define (defects)
  (define head (hash 'INITIAL 0 'CURRENT 0 'PAYOFF 0))
  (define body (hash 0 (state 'D (hash 'C 0 'D 0))))
  (automaton head body))

(define (tit-for-tat)
  (define head (hash 'INITIAL 0 'CURRENT 0 'PAYOFF 0))
  (define body (hash 0 (state 'C (hash 'C 0 'D 1))
                     1 (state 'D (hash 'C 0 'D 1))))
  (automaton head body))

(define (grim-trigger)
  (define head (hash 'INITIAL 0 'CURRENT 0 'PAYOFF 0))
  (define body (hash 0 (state 'C (hash 'C 0 'D 1))
                     1 (state 'D (hash 'C 1 'D 1))))
  (automaton head body))

;; MUTATION
(define (mutate-marginally a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (define mutate-initial (random l))
  (define mutate-state (random l))
  (match-define (state action dispatch) (hash-ref body mutate-state))
  (define r (random 3))
  (define new-head
    (cond [(zero? r) (hash-set head 'INITIAL mutate-initial)]
          [else head])) ; leave unchanged
  (define new-body
    (cond [(zero? r) body] ; leave unchanged
          [(= r 1)
           (hash-set body mutate-state
                     (state (random-action) dispatch))]
          [(= r 2)
           (hash-set body mutate-state
                     (state action
                            (hash-set dispatch (random-action) (random l))))]))
  (automaton new-head new-body))


(define (add-state a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (define (make-transition)
    (hash 'C (random (+ l 1))
          'D (random (+ l 1))))
  (define (make-state) (state (random-action) (make-transition)))
  (define mutate-state (random l))
  (match-define (state action dispatch) (hash-ref body mutate-state))
  (define new-body
    (hash-union
     (hash-set body mutate-state
              (state action
                     (hash-set dispatch (random-action) l)))
     (hash l (make-state))))
  (automaton head new-body))

(define (random-mem l)
  (list-ref l (random (length l))))

(define (detach-state a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (cond
   [(= l 1) (mutate-marginally a)]
   [else (begin
           (define (random-but n r)
             (random-mem (remq mutate-state (build-list n values))))
           (define mutate-state (random l))
           (define (check-rule rule)
             (match-define (cons opponent-action reaction) rule)
             (if (= mutate-state reaction)
                 (cons opponent-action (random-but l mutate-state))
                 rule))
           (define (check-dispatch rules)
             (apply hash
                    (map check-rule (hash->list rules))))
           (define (check-state a-state)
             (match-define (state action rules) a-state)
             (struct-copy state a-state [dispatch (check-dispatch rules)]))
           (define new-body
             (for/list([i (in-range l)])
               (list i
                     (check-state (hash-ref body i)))))
           (automaton head (apply hash (flatten new-body))))]))



(define (mutate a)
  (define r (random 3))
  (cond [(zero? r) (mutate-marginally a)]
        [(= r 1) (add-state a)]
        [(= r 2) (detach-state a)]))
