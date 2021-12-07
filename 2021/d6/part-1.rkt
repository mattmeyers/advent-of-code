#lang racket

(require "common.rkt")

(define (step fs)
    (flatten (map (lambda (f) (if (= f 0) (list 6 8) (- f 1))) fs)))

(define (n-steps n fs)
    (if (= n 0) fs (n-steps (- n 1) (step fs))))

(define (part-1 filename)
    (length (n-steps 80 (get-input filename))))

(provide part-1)

(printf "test-input: ~a~%" (part-1 "test-input.txt"))
(printf "input: ~a~%" (part-1 "input.txt"))