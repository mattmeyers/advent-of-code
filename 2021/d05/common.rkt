#lang racket

(provide
    coord-x
    coord-y
    read-input
    parse-input
    generate-cells
    aggregate
    count-danger)

(struct coord (x y) #:transparent)

(struct delta (dx dy) #:transparent)

(define (new-coord vs)
    (coord (string->number (car vs)) (string->number (last vs))))

(define (coord=? a b)
    (and (coord-x=? a b) (coord-y=? a b)))

(define (coord-x=? a b) (= (coord-x a) (coord-x b)))
(define (coord-y=? a b) (= (coord-y a) (coord-y b)))

(define (add-delta c del)
    (coord
        (+ (coord-x c) (delta-dx del))
        (+ (coord-y c) (delta-dy del))))

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines)
    (map parse-line lines))

(define (parse-line line)
    (define parts (string-split line " -> "))
    (cons 
        (new-coord (string-split (first parts) ",")) 
        (new-coord (string-split (last parts) ","))))

(define (expand start end)
    (if (coord=? start end)
        (list end)
        (cons 
            start 
            (expand (add-delta start (get-delta start end)) end))))

(define (get-delta curr dest)
    (delta 
        (cond 
            [(< (coord-x curr) (coord-x dest)) 1]
            [(> (coord-x curr) (coord-x dest)) -1]
            [else 0])
        (cond 
            [(< (coord-y curr) (coord-y dest)) 1]
            [(> (coord-y curr) (coord-y dest)) -1]
            [else 0])))

(define (generate-cells pairs)
    (if (empty? pairs)
        empty
        (cons (expand (caar pairs) (cdar pairs)) (generate-cells (cdr pairs)))))


(define (aggregate coords [ht #hash()])
    (if (empty? coords)
        ht
        (aggregate (cdr coords) (coord-add1 ht (car coords)))))
    
(define (coord-add1 ht c) 
    (if (hash-has-key? ht c)
        (hash-set ht c (add1 (hash-ref ht c)))
        (hash-set ht c 1)))

(define (count-danger ht)
    (count (lambda (v) (> v 1)) (hash-values ht)))