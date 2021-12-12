#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (all-zero? mtx)
    (= 
        0
        (foldl 
            (lambda (row sum) (+ sum (foldl + 0 (vector->list row)))) 
            0
            (vector->list mtx))))

(define (do-step mtx)
    (define foo (step mtx))
    (void))

(define (run mtx [step 0])
    (define zero? (all-zero? mtx))
    (do-step mtx)
    (if (boolean=? #t zero?)
        step
        (run mtx (add1 step))))

(define (part-2 filename)
    (define input (get-input filename))
    (run input))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))