#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (sum-slice ns)
    (foldl + 0 ns))

(define (sum-slices ls)
    (define range (reverse (build-list line-length values)))
    (map (lambda (n) (sum-slice (nth-slice n ls))) range))

(define (least-common ls)
    (map (lambda (l) (if (< l (/ lines 2)) 1 0)) ls))

(define (most-common ls)
    (map (lambda (l) (if (> l (/ lines 2)) 1 0)) ls))

(define (implode ls)
    (foldr string-append "" (map number->string ls)))

(define (multiply ls)
    (* 
        (string->number (implode (least-common ls)) 2)
        (string->number (implode (most-common ls)) 2)))

(define (part-1 filename)
    (multiply
            (sum-slices 
                (line->binary 
                    (read-input filename)))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))
