#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (cost n vs)
    (foldl (lambda (v c) (+ c (abs (- n v)))) 0 vs))

(define (part-1 filename)
    (list-min (costs (parse-input filename) cost)))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))