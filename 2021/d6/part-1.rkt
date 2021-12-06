#lang racket

(require "common.rkt")

(define (step fs)
    (flatten (map (lambda (f) (if (= f 0) (list 6 8) (- f 1))) fs)))

(define (n-steps n fs)
    (if (= n 0) fs (n-steps (- n 1) (step fs))))

(length (n-steps 80 (get-input "input.txt")))