#lang racket

(require racket/runtime-path)
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

(define (part-2 filename)
    (sum-fish (n-steps 256 (build-map (get-input filename)))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))
