#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (filter-winners boards)
    (filter (lambda (b) (not( bingo? b))) boards))

(define (play numbers boards)
    (define winner (winner? boards))
    (if (and (not (empty? winner)) (= (length boards) 1))
        (cons (length numbers) (car boards))
        (play (cdr numbers) (play-round (car numbers) (filter-winners boards)))))

(define (part-2 filename)
    (define input (parse-input (read-input filename)))
    (define winner (play (car input) (cdr input)))
    (score (cdr winner) (final-number (car input) (car winner))))

(provide part-2)

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))
