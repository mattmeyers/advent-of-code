#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (score-char c)
    (cond
        [(char=? c #\)) 3]
        [(char=? c #\]) 57]
        [(char=? c #\}) 1197]
        [(char=? c #\>) 25137]))

(define (score lst)
    (foldl + 0 (map score-char lst)))

(define (part-1 filename)
    (define input (read-input filename))
    (score (filter (lambda (n) n) (map corrupt? (map string->list input)))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))
