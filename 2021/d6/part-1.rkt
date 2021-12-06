#lang racket

(define (read-input filename)
    (file->string filename))

(define (parse-input s)
    (map string->number (string-split s ",")))

(define (step fs)
    (flatten (map (lambda (f) (if (= f 0) (list 6 8) (- f 1))) fs)))

(define (n-steps n fs)
    (if (= n 0) fs (n-steps (- n 1) (step fs))))

(length (n-steps 80 (parse-input (read-input "input.txt"))))