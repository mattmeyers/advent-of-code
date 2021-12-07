#lang racket

(require racket/runtime-path)
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

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))
