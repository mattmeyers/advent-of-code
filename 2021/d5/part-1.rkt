#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (filter-out-diagonals pairs)
    (filter 
        (lambda (p) 
            (or 
                (= (coord-x (car p)) (coord-x (cdr p)))
                (= (coord-y (car p)) (coord-y (cdr p)))))
        pairs))

(define (part-1 filename)
    (count-danger
        (aggregate
            (flatten
                (generate-cells
                    (filter-out-diagonals
                        (parse-input (read-input filename))))))))

(provide part-1)

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))
