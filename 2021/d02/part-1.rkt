#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (calculate-position ds)
    (foldr (lambda (del pos) (
        cond
            (
                (string=? (delta-direction del) "forward") 
                (position 
                    (position-vertical pos) 
                    (+ (position-horizontal pos) (delta-distance del))))
            (
                (string=? (delta-direction del) "up") 
                (position 
                    (- (position-vertical pos) (delta-distance del)) 
                    (position-horizontal pos)))
            (
                (string=? (delta-direction del) "down") 
                (position 
                    (+ (position-vertical pos) (delta-distance del)) 
                    (position-horizontal pos)))
            (else pos)
    )) (position 0 0) ds))

(define (part-1 filename)
    (position-product (calculate-position (build-deltas (read-input filename)))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))
