#lang typed/racket

(provide part-1)

(: read-input (-> String (Listof Integer)))
(define (read-input filename)
    (file->list filename))

(: zip-diff (-> (Listof Integer) (Listof Integer)))
(define (zip-diff l)
    (map - (cdr l) (drop-right l 1)))

(: count-positives (-> (Listof Integer) Integer))
(define (count-positives l)
    (length (filter positive? l)))

(: part-1 (-> Integer))
(define (part-1)
     (count-positives (zip-diff (read-input "input.txt" ))))

(part-1)