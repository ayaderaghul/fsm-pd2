;; License

;; Copyright © 2012 Dave Herman
;; https://github.com/dherman/csv-write.rkt
;; Licensed under the MIT License.


(module csv-write mzscheme
  (require (lib "string.ss" "srfi" "13")
           (prefix string: (lib "string.ss"))
           (lib "contract.ss")
           (lib "etc.ss"))
  (define (normalize-newlines s)
    (string-join (string:regexp-split #rx"(?:\r\n)|\r|\n" s) "\r\n"))
  (define (normalize-quotes s)
    (regexp-replace* #rx"\"" s "\"\""))
  (define (format-cell x)
    (let ([str (format "~a" x)])
      (if (regexp-match #rx"\"|,|\r|\n" str)
          (string-append "\"" (normalize-newlines (normalize-quotes str)) "\"")
          str)))
  (define (format-row ls)
    (string-join (map format-cell ls) "," 'infix))
  (define (format-table ls)
    (map format-row ls))
  (define write-cell
    (opt-lambda (x [out (current-output-port)])
      (display (format-cell x) out)))
  (define write-row
    (opt-lambda (ls [out (current-output-port)])
      (display (format-row ls) out)
      (newline out)))
  (define write-table
    (opt-lambda (ls [out (current-output-port)])
      (for-each (lambda (row)
                  (display row out)
                  (newline out))
                (format-table ls))))
  (provide/contract
   [format-cell (any/c . -> . string?)]
   [format-row ((listof any/c) . -> . string?)]
   [format-table ((listof (listof any/c)) . -> . (listof string?))]
   [write-cell ((any/c) (output-port?) . opt-> . any)]
   [write-row (((listof any/c)) (output-port?) . opt-> . any)]
   [write-table (((listof (listof any/c))) (output-port?) . opt-> . any)]))
