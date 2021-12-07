#lang racket

(struct position (vertical horizontal) #:transparent)

(struct delta (direction distance) #:transparent)

(define (read-input filename)
    (file->lines filename))

(define (split-lines ls)
    (map (lambda (l) (string-split l " ")) ls))

(define (build-deltas ls)
    (map (lambda (v) (delta (car v) (string->number (last v)))) (split-lines ls)))

(define (position-product pos) 
    (* (position-vertical pos) (position-horizontal pos)))

(provide
    position
    position-vertical
    position-horizontal
    delta-direction
    delta-distance
    read-input
    split-lines
    build-deltas
    position-product)