#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (part-2 filename)
    (count-danger
        (aggregate
            (flatten
                (generate-cells
                    (parse-input (read-input filename)))))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))
