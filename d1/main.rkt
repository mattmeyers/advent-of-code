#lang racket

(define (zip-diff l)
    (map - (cdr l) (reverse(cdr(reverse l)))))

(define (part-1)
    (length (filter positive? (zip-diff (file->list "input.txt" )))))

(part-1)