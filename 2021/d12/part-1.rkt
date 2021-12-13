#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (update-visited visited v)
    (if (small-cave? v) (set-add visited v) visited))

(define (get-children al curr visited)
    (filter-not 
        (lambda (n) (set-member? visited n))
        (hash-ref al curr)))

(define (build-routes al visited curr)
    (if (string=? curr "end")
        1
        (foldl 
            (lambda (n sum) (+ sum (build-routes al (update-visited visited curr) n)))
            0
            (get-children al curr visited))))

(define (part-1 filename)
    (define al (make-adjacency-list filename))
    (build-routes al (set) "start"))

(define-runtime-path test-input-1-file "test-input-1.txt")
(define-runtime-path test-input-2-file "test-input-2.txt")
(define-runtime-path test-input-3-file "test-input-3.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input 1: ~a~%" (part-1 (path->string test-input-1-file)))
(printf "Test Input 2: ~a~%" (part-1 (path->string test-input-2-file)))
(printf "Test Input 3: ~a~%" (part-1 (path->string test-input-3-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))