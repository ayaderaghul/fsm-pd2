#lang racket
(provide load-dynamics out-data out-mean out-rank configuration-string)
(require "csv.rkt" 2htdp/batch-io plot)

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

(define (out-rank filename day data)
  (out-data filename (append (list (list day)
                                 (map list data)))))

(define (configuration-string N speed rounds delta)
  (format
   "N = ~a, speed = ~a, rounds = ~a, delta = ~a"
   N speed rounds delta))






