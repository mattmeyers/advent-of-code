#lang typed/racket

(require racket/runtime-path)
(require "common.rkt")

(: windowify (-> (Listof Integer) (Listof Integer)))
(define (windowify l)
    (if (= (length l) 3)
        (list (sum l))
        (append (list (sum (take l 3))) (windowify (cdr l)))))

(: part-2 (-> String Integer))
(define (part-2 filename)
    (count-positives (zip-diff (windowify (read-input filename)))))

(provide part-2)
(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))
