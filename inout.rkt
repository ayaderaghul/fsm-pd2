#lang racket
(provide (all-defined-out))
(require "csv.rkt" "scan.rkt" 2htdp/batch-io plot)
(require (planet neil/csv:2:0))
;; IMPORT
(define (load-data csv-file)
  [define strings (read-csv-file csv-file)]
  [define l (length strings)]
  [define-values (data)
    (for/fold ([data '()])
              ([i (in-range l)])
      [define datum (apply string->number (list-ref strings i))]
      (values (cons datum data)))]
  (reverse data))

;; load dynamics
(define (pack-coors a-list)
  [define l (length a-list)]
  (for/list ([i (in-range (/ l 2))])
    (list
     (list-ref a-list (* 2 i))
     (list-ref a-list (add1 (* 2 i))))))
(define (load-dynamic csv-file)
  (lines (pack-coors (load-data csv-file))))
(define (load-dynamics file-list)
  [define l (length file-list)]
  (for/list ([i (in-range l)])
    (load-dynamic (list-ref file-list i))))

;; EXPORT DATA
;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-mean filename data)
  (out-data filename (map list data)))

(define (configuration-string N speed rounds delta)
  (format
   "N = ~a, speed = ~a, rounds = ~a, delta = ~a"
   N speed rounds delta))

(define (export-automata rankings rank-file)
  (define to-export
    (remove* (list #f)
             (for/list ([(key value) (in-hash rankings)])
               (and
                (> value 5)
                (list key value)))))
  (if (false? (first to-export))
      (out-data rank-file (list (list "")))
      (out-data rank-file (list (apply append to-export)))))

(define (out-rank day rankings rank-file)
(out-data rank-file (list (list (number->string day))))
(export-automata rankings rank-file))
  

;; IMPORT AUTOMATA

(define make-automaton-csv-reader
  (make-csv-reader-maker
   '((separator-chars #\,)
     (strip-leading-whitespace? . #t)
     (strip-trailing-whitespace? . #t))))

(define (all-rows file make-reader)
  (define next-row
    (make-reader (open-input-file file)))
  (define (loop)
    (define row (next-row))
    (if (empty? row) '()
        (cons row (loop))))
  (loop))

(define (at-row n file make-reader)
  (define next-row (make-reader (open-input-file file)))
  (define (at x)
    (define row (next-row))
    (if (zero? x) row (at (- x 1))))
  (at n))

(define (take-odd lst)
  (define l (length lst))
  (filter-not false?
              (for/list ([i (in-range l)]
                         [j (in-list lst)])
                (and (odd? i) j))))

(define (take-even lst)
  (define l (length lst))
  (filter-not void?
              (for/list ([i (in-range l)]
                         [j (in-list lst)])
                (if (even? i) j (void)))))

(define (convert x)
  (cond [(< (string-length x) 4) (string->number x)]
        [else
         (define code (string-trim (string-trim x "(") ")"))
         (define pieces (string-split code " "))
         (define au (map string->number pieces))
         (recover-automaton au)]))

(define (resurrect-at cycle pre file make-reader)
  (define posn (+ 1 (* cycle 2)))
  (define data (at-row posn file make-reader))
  (resurrect pre data))

(define (resurrect pre lst)
  (define table (map convert lst))
  (define resurrected (take-even table))
  (define l (length resurrected))
  (define names (generate-ax pre (build-list l values)))
  (for ([i (in-list names)]
        [j (in-list resurrected)])
    (eval (list 'define i j)))
  resurrected)

(define (x->ax pre x)
  (string->symbol (string-append pre (number->string x))))
(define (generate-ax pre a-list)
  (map (lambda (x) (x->ax pre x)) a-list))
