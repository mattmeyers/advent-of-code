#lang racket

(require "common.rkt")

(define initial-map #hash(
    (0 . 0)
    (1 . 0)
    (2 . 0)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . 0)
    (7 . 0)
    (8 . 0)))

(define (build-map fs [ht initial-map])
    (if (empty? fs)
        ht
        (build-map (cdr fs) (hash-set ht (car fs) (add1 (hash-ref ht (car fs)))))))

(define (step ht)
    (hash
        0  (hash-ref ht 1)
        1  (hash-ref ht 2)
        2  (hash-ref ht 3)
        3  (hash-ref ht 4)
        4  (hash-ref ht 5)
        5  (hash-ref ht 6)
        6  (+ (hash-ref ht 7) (hash-ref ht 0))
        7  (hash-ref ht 8)
        8  (hash-ref ht 0)))

(define (n-steps n fs)
    (if (= n 0) fs (n-steps (- n 1) (step fs))))

(define (sum-fish ht)
    (foldr + 0 (hash-values ht)))

(sum-fish (n-steps 256 (build-map (get-input "input.txt"))))