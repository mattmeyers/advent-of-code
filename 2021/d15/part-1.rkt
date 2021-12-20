#lang racket

(struct vertex (pos weight) #:transparent)

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines [i 0])
    (if (empty? lines)
        empty
        (append
            (parse-row (string->list (car lines)) i)
            (parse-input (cdr lines) (add1 i)))))

(define (parse-row row i [j 0])
    (if (empty? row)
        empty
        (append
            (list (vertex (cons i j) (string->number (string (car row)))))
            (parse-row (cdr row) i (add1 j)))))

(define (get-input filename)
    (parse-input (read-input filename)))

(define (min-vertex spt vs m)
    (if (empty? vs)
        m
        (min-vertex 
            (cdr vs) 
            (if (hash-has-key? (car vs))
                m
                (if (< (vertex-weight (car vs)) (vertex-weight m)) (car vs) m)))))

(define (dijkstra vertices spt graph)
    '())

(get-input "test-input.txt")
