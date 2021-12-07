#lang typed/racket

(require "common.rkt")

(: windowify (-> (Listof Integer) (Listof Integer)))
(define (windowify l)
    (if (= (length l) 3)
        (list (sum l))
        (append (list (sum (take l 3))) (windowify (cdr l)))))

(: part-2 (-> String Integer))
(define (part-2 filename)
    (count-positives (zip-diff (windowify (read-input filename)))))

(provide part-2)

(part-2 "test-input.txt")
(part-2 "input.txt")