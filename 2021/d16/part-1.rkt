#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (sum-versions ps)
    (foldl 
        (λ (p s) 
            (+ 
                s 
                (bin->decimal (packet-version p)) 
                (if (list? (packet-data p)) 
                    (sum-versions (packet-data p)) 
                    0))) 
        0 
        ps))

(define (part-1 filename)
    (sum-versions (parse-packets (get-input filename))))

(define-runtime-path test-input-file-1 "test-input-1-1.txt")
(define-runtime-path test-input-file-2 "test-input-1-2.txt")
(define-runtime-path test-input-file-3 "test-input-1-3.txt")
(define-runtime-path test-input-file-4 "test-input-1-4.txt")
(define-runtime-path test-input-file-5 "test-input-1-5.txt")
(define-runtime-path test-input-file-6 "test-input-1-6.txt")
(define-runtime-path test-input-file-7 "test-input-1-7.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input 1: ~a~%" (part-1 (path->string test-input-file-1)))
(printf "Test Input 2: ~a~%" (part-1 (path->string test-input-file-2)))
(printf "Test Input 3: ~a~%" (part-1 (path->string test-input-file-3)))
(printf "Test Input 4: ~a~%" (part-1 (path->string test-input-file-4)))
(printf "Test Input 5: ~a~%" (part-1 (path->string test-input-file-5)))
(printf "Test Input 6: ~a~%" (part-1 (path->string test-input-file-6)))
(printf "Test Input 7: ~a~%" (part-1 (path->string test-input-file-7)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))