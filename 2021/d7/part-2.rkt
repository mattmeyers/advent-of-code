#lang racket

(define (read-input filename)
    (file->string filename))

(define (parse-input filename)
    (map string->number (string-split (read-input filename) ",")))

(define (list-min lst)
    (foldl min (car lst) lst))

(define (list-max lst)
    (foldl max (car lst) lst))

(define (possible-values vs)
    (range (list-min vs) (list-max vs)))

(define (sum-n n)
    (/ (* n (add1 n)) 2))

(define (cost n vs)
    (foldl (lambda (v c) (+ c (sum-n (abs (- n v))))) 0 vs))

(define (costs positions)
    (let [(r (possible-values positions))]
        (map (lambda (n) (cost n positions)) r)))

(list-min (costs (parse-input "input.txt")))

