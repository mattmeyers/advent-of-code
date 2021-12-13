#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (update-visited visited v)
    (if (small-cave? v) 
        (hash-set visited v (add1 (hash-ref visited v 0))) 
        visited))

(define (can-visit-again? visited)
    (define values (hash-values visited))
    (= (length values) (foldl + 0 values)))

(define (get-children al curr visited)
    (filter
        (lambda (n) 
            (or 
                (zero? (hash-ref visited n 0))
                (and (= (hash-ref visited n) 1) (can-visit-again? visited))))
        (hash-ref al curr)))

(define (build-routes al visited curr)
    (define new-visited (update-visited visited curr))
    (if (string=? curr "end")
        1
        (foldl 
            (lambda (n sum) (+ sum (build-routes al new-visited n)))
            0
            (get-children al curr new-visited))))

(define (part-2 filename)
    (define al (make-adjacency-list filename))
    (build-routes al #hash() "start"))

(define-runtime-path test-input-1-file "test-input-1.txt")
(define-runtime-path test-input-2-file "test-input-2.txt")
(define-runtime-path test-input-3-file "test-input-3.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input 1: ~a~%" (part-2 (path->string test-input-1-file)))
(printf "Test Input 2: ~a~%" (part-2 (path->string test-input-2-file)))
(printf "Test Input 3: ~a~%" (part-2 (path->string test-input-3-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))