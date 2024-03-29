#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (sum-bits ls)
    (foldr + 1 (map (lambda (n) (if (= n 0) -1 1)) ls)))

(define (most-common-bit ls)
    (if (positive? (sum-bits ls)) 1 0))

(define (least-common-bit ls)
    (if (positive? (sum-bits ls)) 0 1))

(define (filter-rows bit n ls) 
    (filter (lambda (l) (= bit (bitwise-bit-field l n (add1 n)))) ls))

(define (run n ls f)
    (define list-len (length ls))
    (if (= list-len 1)
        (car ls)
        (run 
            (- n 1)
            (filter-rows (f (nth-slice n ls)) n ls )
            f)))

(define (part-2 filename)
    (define lines (read-input filename))
    (define line-len (string-length (car lines)))
    (define number-lines (line->binary lines))
    (* 
        (run (- line-len 1) number-lines most-common-bit) 
        (run (- line-len 1) number-lines least-common-bit)))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))

(module+ test
    (require rackunit rackunit/text-ui)
    
    (define suite
        (test-suite
            "day 3, part 2"
         
            (test-eqv? "most-common-bit, mostly 1"
                (most-common-bit (list 1 1 0 0 1))
                1)
            (test-eqv? "most-common-bit, mostly 0"
                (most-common-bit (list 1 0 0 0 1))
                0)
            (test-eqv? "most-common-bit, equal"
                (most-common-bit (list 1 0 0 1))
                1)
            (test-eqv? "least-common-bit, mostly 1"
                (least-common-bit (list 1 1 0 0 1))
                0)
            (test-eqv? "least-common-bit, mostly 0"
                (least-common-bit (list 1 0 0 0 1))
                1)
            (test-eqv? "least-common-bit, equal"
                (least-common-bit (list 1 0 0 1))
                0)
            (test-pred "filter-rows, high bit"
                (lambda (v) (equal? (list->set v) (list->set '(#b100 #b101))))
                (filter-rows 1 2 '(#b100 #b101 #b001)))
            (test-pred "filter-rows, middle bit"
                (lambda (v) (equal? (list->set v) (list->set '(#b100 #b101 #b001))))
                (filter-rows 0 1 '(#b100 #b101 #b001)))
            (test-eqv? "full run"
                (part-2 (read-input "test-input.txt"))
                230)
         ))
         
    (run-tests suite))