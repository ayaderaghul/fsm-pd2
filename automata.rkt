#lang racket
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
  (define (make-head) (make-hash (list
                                  (cons 'INITIAL initial-current)
                                  (cons 'CURRENT initial-current)
                                  (cons 'PAYOFF 0))))
  (define ids (build-list states# values))
  (define (make-body) (make-hash (map cons ids (make-states))))
  (define (make-states) (build-list states# make-state))
  (define (make-state _) (state (random-action) (make-transition)))
  (define (make-transition)
    (make-hash (list
                (cons 'C (random states#))
                (cons 'D (random states#)))))
  (define body (make-hash (map cons ids (make-states))))
  (automaton (make-head) (make-body)))

(define (reset! a) ; reset
  (match-define (automaton head body) a)
  (hash-set! head 'CURRENT (hash-ref head 'INITIAL)))

;; CLASSIC AUTOMATA
(define (cooperates)
  (define head (make-hash (list
                           (cons 'INITIAL 0)
                           (cons 'CURRENT 0)
                           (cons 'PAYOFF 0))))
  (define body (make-hash (list (cons 0
                                      (state 'C
                                             (make-hash (list
                                                         (cons 'C 0)
                                                         (cons 'D 0))))))))
  (automaton head body))

(define (defects)
  (define head (make-hash (list
                           (cons 'INITIAL 0)
                           (cons 'CURRENT 0)
                           (cons 'PAYOFF 0))))
  (define body (make-hash (list (cons 0
                                      (state 'D
                                             (make-hash (list
                                                         (cons 'C 0)
                                                         (cons 'D 0))))))))
  (automaton head body))

(define (tit-for-tat)
  (define head (make-hash (list
                           (cons 'INITIAL 0)
                           (cons 'CURRENT 0)
                           (cons 'PAYOFF 0))))
  (define body (make-hash (list
                           (cons 0
                                 (state 'C
                                        (make-hash (list
                                                    (cons 'C 0)
                                                    (cons 'D 1)))))
                           (cons 1
                                 (state 'D
                                        (make-hash (list
                                                    (cons 'C 0)
                                                    (cons 'D 1))))))))
  (automaton head body))

(define (grim-trigger)
  (define head (make-hash (list
                           (cons 'INITIAL 0)
                           (cons 'CURRENT 0)
                           (cons 'PAYOFF 0))))
  (define body (make-hash (list (cons 0
                                      (state 'C
                                             (make-hash (list
                                                         (cons 'C 0)
                                                         (cons 'D 1)))))
                                (cons 1 (state 'D
                                               (make-hash (list
                                                           (cons 'C 1)
                                                           (cons 'D 1))))))))
  (automaton head body))

;; MUTATION
(define (mutate-marginally a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (define mutate-initial (random l))
  (define mutate-state (random l))
  (match-define (state action dispatch) (hash-ref body mutate-state))
  (define r (random 3))
  (cond [(zero? r) (hash-set! head 'INITIAL mutate-initial)]
        [(= r 1)
         (hash-set! body mutate-state
                      (state (random-action) dispatch))]
        [(= r 2)
         (hash-set! dispatch (random-action) (random l))]))

(define (add-state a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (define (make-transition)
    (make-hash (list
                (cons 'C (random (+ l 1)))
                (cons 'D (random (+ l 1))))))
  (define (make-state) (state (random-action) (make-transition)))
  (define r (random l))
  (match-define (state action dispatch) (hash-ref body r))
  (hash-set! dispatch (random-action) l)
  (hash-set*! body l (make-state))
  )

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
           (define (check-value rules index)
             (and
              (= mutate-state (hash-iterate-value rules index))
              (hash-set! rules
                         (hash-iterate-key rules index)
                         (random-but l mutate-state))))
           (define (check-state a-state)
             (begin
               (check-value (state-dispatch a-state) 3)
               (check-value (state-dispatch a-state) 6)))
           (map check-state (map cdr (hash->list body))))]))

;; why hash-map dosent work

(define (mutate a)
  (define r (random 3))
  (cond [(zero? r) (mutate-marginally a)]
        [(= r 1) (add-state a)]
        [(= r 2) (detach-state a)]))
