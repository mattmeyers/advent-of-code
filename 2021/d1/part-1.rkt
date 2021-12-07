#lang typed/racket

(require "common.rkt")

(: part-1 (-> String Integer))
(define (part-1 filename)
     (count-positives (zip-diff (read-input filename ))))

(provide part-1)

(printf "test-input: ~a~%" (part-1 "test-input.txt"))
(printf "input: ~a~%" (part-1 "input.txt"))