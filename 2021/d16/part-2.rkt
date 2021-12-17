#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (execute node)
    (cond
        [(string=? "000" (packet-type node)) 
            (foldl (λ (n acc) (+ acc (execute n))) 0 (packet-data node))]
        [(string=? "001" (packet-type node)) 
            (foldl (λ (n acc) (* acc (execute n))) 1 (packet-data node))]
        [(string=? "010" (packet-type node)) 
            (inexact->exact (foldl (λ (n acc) (min (execute n) acc)) +inf.0 (packet-data node)))]
        [(string=? "011" (packet-type node)) 
            (inexact->exact (foldl (λ (n acc) (max (execute n) acc)) -inf.0 (packet-data node)))]
        [(string=? "100" (packet-type node)) (bin->decimal (packet-data node))]
        [(string=? "101" (packet-type node))
            (let ([d (packet-data node)]) (if (> (execute (first d)) (execute (last d))) 1 0))]
        [(string=? "110" (packet-type node)) 
            (let ([d (packet-data node)]) (if (< (execute (first d)) (execute (last d))) 1 0))]
        [(string=? "111" (packet-type node)) 
            (let ([d (packet-data node)]) (if (= (execute (first d)) (execute (last d))) 1 0))]))

(define (part-2 filename)
    (execute (car (parse-packets (get-input filename)))))

(define-runtime-path test-input-file-1 "test-input-2-1.txt")
(define-runtime-path test-input-file-2 "test-input-2-2.txt")
(define-runtime-path test-input-file-3 "test-input-2-3.txt")
(define-runtime-path test-input-file-4 "test-input-2-4.txt")
(define-runtime-path test-input-file-5 "test-input-2-5.txt")
(define-runtime-path test-input-file-6 "test-input-2-6.txt")
(define-runtime-path test-input-file-7 "test-input-2-7.txt")
(define-runtime-path test-input-file-8 "test-input-2-8.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input 1: ~a~%" (part-2 (path->string test-input-file-1)))
(printf "Test Input 2: ~a~%" (part-2 (path->string test-input-file-2)))
(printf "Test Input 3: ~a~%" (part-2 (path->string test-input-file-3)))
(printf "Test Input 4: ~a~%" (part-2 (path->string test-input-file-4)))
(printf "Test Input 5: ~a~%" (part-2 (path->string test-input-file-5)))
(printf "Test Input 6: ~a~%" (part-2 (path->string test-input-file-6)))
(printf "Test Input 7: ~a~%" (part-2 (path->string test-input-file-7)))
(printf "Test Input 8: ~a~%" (part-2 (path->string test-input-file-8)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))