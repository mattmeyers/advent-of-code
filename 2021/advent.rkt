#! /usr/bin/env racket
#lang racket

(define (normalize-day d)
    (if (= 1 (string-length d))
        (format "0~a" d)
        d))

(define advent
    (command-line
    #:args (day part)
    (cons (normalize-day day) part)))

(define code-filename
    (format "./d~a/part-~a.rkt" (car advent) (cdr advent)))

(dynamic-require code-filename #f)
