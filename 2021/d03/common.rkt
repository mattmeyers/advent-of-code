#lang racket

(provide 
    line-length
    lines
    read-input
    line->binary
    nth-slice)

(define line-length 12)
(define lines 1000)

(define (read-input filename)
    (file->lines filename))

(define (line->binary ls)
    (map (lambda (l) (string->number l 2)) ls))

(define (nth-slice n ls)
    (map (lambda (l) (bitwise-bit-field l n (+ n 1))) ls))