#lang racket

(require "pd-automata.rkt" "pd-population.rkt" "pd-scan.rkt" "pd-inout.rkt" plot)
(plot-new-window? #t)
(provide (all-defined-out))
(define N 1000)
(define CYCLES 400)
(define SPEED 100)
(define DELTA .95)
(define ROUNDS 100)

(define (build-population d t c)
  (define p
    (append
    (build-list d (lambda (_) (defects)))
    (build-list t (lambda (_) (tit-for-tat)))
    (build-list c (lambda (_) (cooperates)))))
  (list->vector (shuffle p)))

(define point-list
  (list
   (list 50 50 900)
   (list 50 100 850)
   (list 50 150 800)
   (list 50 200 750)
   (list 50 250 700)
   (list 50 300 650)
   (list 50 350 500)
   (list 50 400 550)
   (list 50 450 500)))

(define (evolve-rd population cycles speed rounds delta)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-population population rounds delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (cons (scan-defects-tft p3)
               (evolve-rd p3 (- cycles 1) speed rounds delta))]))

(define (test1 test-point file-name)
  (collect-garbage)
  (define A (apply build-population test-point))
  (define rd-types
    (time (evolve-rd A CYCLES SPEED ROUNDS DELTA)))
  (out-data file-name (map list (flatten rd-types)))
  (define rd (lines rd-types))
  (plot rd #:x-min 0.0 #:x-max N #:y-min 0 #:y-max N #:title "rd"))

(define (test1s test-points)
  (for ([i (length test-points)])
    (test1 (list-ref test-points i)
           (string-append "rd" (number->string i)))))

(define file-list
  (list "rd0" "rd1" "rd2" "rd3" "rd4" "rd5" "rd6" "rd7" "rd8"))

(define (plot-dynamics file-list)
  (define data (load-dynamics file-list))
  (plot data
        #:x-max N #:y-max N
        #:x-label "defects" #:y-label "tft" #:title "delta 1"))

(define (main)
  (collect-garbage)
  (test1s point-list)
  (plot-dynamics file-list))
