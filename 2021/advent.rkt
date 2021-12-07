#! /usr/bin/env racket
#lang racket

(define advent
    (command-line
    #:args (day part)
    (cons day part)))

(define code-filename
    (format "./d~a/part-~a.rkt" (car advent) (cdr advent)))

(dynamic-require code-filename #f)
