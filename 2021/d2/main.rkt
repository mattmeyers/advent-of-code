#lang racket

(struct position (vertical horizontal) #:transparent)

(struct delta (direction distance) #:transparent)

(define (read-input filename)
    (file->lines filename))

(define (split-lines ls)
    (map (lambda (l) (string-split l " ")) ls))

(define (build-deltas ls)
    (map (lambda (v) (delta (car v) (last v))) (split-lines ls)))

(define (calculate-position ds)
    (foldr (lambda (del pos) (
        cond
            (
                (string=? (delta-direction del) "forward") 
                (position (position-vertical pos) (+ (position-horizontal pos) (string->number (delta-distance del))))
            )
            (
                (string=? (delta-direction del) "up") 
                (position (- (position-vertical pos) (string->number (delta-distance del))) (position-horizontal pos))
            )
            (
                (string=? (delta-direction del) "down") 
                (position (+ (position-vertical pos) (string->number (delta-distance del))) (position-horizontal pos))
            )
            (else pos)
    )) (position 0 0) ds))

(define (position-product pos) 
    (* (position-vertical pos) (position-horizontal pos)))

(define (part-1)
    (position-product (calculate-position (build-deltas (read-input "input.txt")))))

(part-1)