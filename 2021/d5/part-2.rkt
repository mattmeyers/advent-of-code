#lang racket

(require "common.rkt")

(define (part-2)
    (count-danger
        (aggregate
            (flatten
                (generate-cells
                    (parse-input (read-input "input.txt")))))))


(part-2)