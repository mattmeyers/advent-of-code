#lang racket

(require "common.rkt")

(define (part-2 filename)
    (count-danger
        (aggregate
            (flatten
                (generate-cells
                    (parse-input (read-input filename)))))))

(provide part-2)

(printf "test-input: ~a~%" (part-2 "test-input.txt"))
(printf "input: ~a~%" (part-2 "input.txt"))