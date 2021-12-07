#lang racket

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

(provide part-1)

(printf "test-input: ~a~%" (part-1 "test-input.txt"))
(printf "input: ~a~%" (part-1 "input.txt"))
