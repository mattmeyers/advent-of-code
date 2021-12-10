#lang racket

(require racket/runtime-path)
(require "common.rkt")

(define (not-corrupt? line)
    (not (corrupt? line)))

(define (simplify cs [stack empty])
    (if (empty? cs)
        stack
        (cond
            [(opens? (car cs)) (simplify (cdr cs) (cons (car cs) stack))]
            [(closes? (car stack) (car cs)) (simplify (cdr cs) (cdr stack))])))

(define (map-char c)
    (cond
        [(char=? c #\() #\)]
        [(char=? c #\[) #\]]
        [(char=? c #\{) #\}]
        [(char=? c #\<) #\>]))

(define (complete stack)
    (map map-char stack))

(define (score-char c)
    (cond
        [(char=? c #\)) 1]
        [(char=? c #\]) 2]
        [(char=? c #\}) 3]
        [(char=? c #\>) 4]))

(define (score lst [s 0])
    (if (empty? lst)
        s
        (score (cdr lst) (+ (* s 5) (score-char (car lst))))))

(define (middle-score scores)
    (car (drop (sort scores <) (quotient (length scores) 2))))

(define (part-2 filename)
    (define input (read-input filename))
    (middle-score (map score (map complete (map simplify (filter not-corrupt? (map string->list input)))))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))
