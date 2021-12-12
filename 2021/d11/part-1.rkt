#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (run mtx [steps 1])
    (foldl (lambda (n c) (+ c (step mtx))) 0 (range steps)))

(define (part-1 filename)
    (define input (get-input filename))
    (run input 100))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))