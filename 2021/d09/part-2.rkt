#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (line->ranges lst [start 0] [end 1])
    (if (empty? lst)
        empty
        (cons)))

(define (part-2 filename)
    (define input (get-input filename))
    '())

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
; (printf "Full Input: ~a~%" (part-2 (path->string input-file)))