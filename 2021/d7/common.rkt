#lang racket

(provide
    parse-input
    costs
    list-min)

(define (read-input filename)
    (file->string filename))

(define (parse-input filename)
    (map string->number (string-split (read-input filename) ",")))

(define (list-min lst)
    (apply min lst))

(define (list-max lst)
    (apply max lst))

(define (possible-values vs)
    (range (list-min vs) (list-max vs)))

(define (costs positions cost-fn)
    (let [(r (possible-values positions))]
        (map (lambda (n) (cost-fn n positions)) r)))