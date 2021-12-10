#lang racket

(provide get-input)

(define (read-input filename)
    (file->string filename))

(define (parse-input s)
    (map string->number (string-split s ",")))

(define (get-input filename)
    (parse-input (read-input filename)))