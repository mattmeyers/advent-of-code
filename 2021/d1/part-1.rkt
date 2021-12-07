#lang typed/racket

(require racket/runtime-path)
(require "common.rkt")

(: part-1 (-> String Integer))
(define (part-1 filename)
     (count-positives (zip-diff (read-input filename ))))

(provide part-1)

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))
