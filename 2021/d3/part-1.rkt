#lang racket

(require "common.rkt")

(define (sum-slice ns)
    (foldl + 0 ns))

(define (sum-slices ls)
    (define range (reverse (build-list line-length values)))
    (map (lambda (n) (sum-slice (nth-slice n ls))) range))

(define (least-common ls)
    (map (lambda (l) (if (< l (/ lines 2)) 1 0)) ls))

(define (most-common ls)
    (map (lambda (l) (if (> l (/ lines 2)) 1 0)) ls))

(define (implode ls)
    (foldr string-append "" (map number->string ls)))

(define (multiply ls)
    (* 
        (string->number (implode (least-common ls)) 2)
        (string->number (implode (most-common ls)) 2)))

(multiply
        (sum-slices 
            (line->binary 
                (read-input "input.txt"))))