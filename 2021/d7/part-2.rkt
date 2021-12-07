#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (sum-n n)
    (/ (* n (add1 n)) 2))

(define (cost n vs)
    (foldl (lambda (v c) (+ c (sum-n (abs (- n v))))) 0 vs))

(define (part-2 filename)
    (list-min (costs (parse-input filename) cost)))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))