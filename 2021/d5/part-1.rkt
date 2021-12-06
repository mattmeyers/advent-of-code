#lang racket

(require "common.rkt")

(define (filter-out-diagonals pairs)
    (filter 
        (lambda (p) 
            (or 
                (= (coord-x (car p)) (coord-x (cdr p)))
                (= (coord-y (car p)) (coord-y (cdr p)))))
        pairs))

(define (part-1)
    (count-danger
        (aggregate
            (flatten
                (generate-cells
                    (filter-out-diagonals
                        (parse-input (read-input "input.txt"))))))))


(part-1)