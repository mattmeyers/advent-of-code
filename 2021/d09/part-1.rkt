#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (filter-low-points mask mtx)
    (if (empty? mask)
        '()
        (append 
            (if (= 1 (car mask)) (list (car mtx)) '())
            (filter-low-points (cdr mask) (cdr mtx)))))

(define (calculate-risk lst)
    (foldl (lambda (n s) (+ s (add1 n))) 0 lst))

(define (part-1 filename)
    (define input (get-input filename))
    (define mask (get-low-point-mask input))
    (calculate-risk (filter-low-points mask (flatten input))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))