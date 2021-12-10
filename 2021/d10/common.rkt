#lang racket

(provide
    read-input
    opens?
    closes?
    corrupt?)

(define (read-input filename)
    (file->lines filename))

(define (opens? c)
    (or 
        (char=? c #\()
        (char=? c #\[)
        (char=? c #\{)
        (char=? c #\<)))

(define (closes? l r)
    (or 
        (and (char=? l #\() (char=? r #\)))
        (and (char=? l #\[) (char=? r #\]))
        (and (char=? l #\{) (char=? r #\}))
        (and (char=? l #\<) (char=? r #\>))))

(define (corrupt? cs [stack empty])
    (if (empty? cs)
        #f
        (cond
            [(opens? (car cs)) (corrupt? (cdr cs) (cons (car cs) stack))]
            [(closes? (car stack) (car cs)) (corrupt? (cdr cs) (cdr stack))]
            [else (car cs)]
            )))