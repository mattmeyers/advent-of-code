#lang racket

(require "common.rkt")

(define (play numbers boards)
    (define winner (winner? boards))
    (if (not (empty? winner))
        (cons (length numbers) winner)
        (play (cdr numbers) (play-round (car numbers) boards))))

(define (part-1)
    (define input (parse-input (read-input "input.txt")))
    (define winner (play (car input) (cdr input)))
    (score (cdr winner) (final-number (car input) (car winner))))

(part-1)
