#lang racket

(require "common.rkt")

(define (filter-winners boards)
    (filter (lambda (b) (not( bingo? b))) boards))

(define (play numbers boards)
    (define winner (winner? boards))
    (if (and (not (empty? winner)) (= (length boards) 1))
        (cons (length numbers) (car boards))
        (play (cdr numbers) (play-round (car numbers) (filter-winners boards)))))

(define (part-2)
    (define input (parse-input (read-input "input.txt")))
    (define winner (play (car input) (cdr input)))
    (score (cdr winner) (final-number (car input) (car winner))))

(part-2)
