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
