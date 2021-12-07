#lang racket

(require "common.rkt")

(define (play numbers boards)
    (define winner (winner? boards))
    (if (not (empty? winner))
        (cons (length numbers) winner)
        (play (cdr numbers) (play-round (car numbers) boards))))

(define (part-1 filename)
    (define input (parse-input (read-input filename)))
    (define winner (play (car input) (cdr input)))
    (score (cdr winner) (final-number (car input) (car winner))))

(provide part-1)

(printf "test-input: ~a~%" (part-1 "test-input.txt"))
(printf "input: ~a~%" (part-1 "input.txt"))