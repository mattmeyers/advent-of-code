#lang typed/racket

(provide 
    read-input
    sum
    zip-diff
    count-positives)

(: read-input (-> String (Listof Integer)))
(define (read-input filename)
    (file->list filename))

(: sum (-> (Listof Integer) Integer))
(define (sum l)
    (foldr + 0 l))

(: zip-diff (-> (Listof Integer) (Listof Integer)))
(define (zip-diff l)
    (map - (cdr l) (drop-right l 1)))

(: count-positives (-> (Listof Integer) Integer))
(define (count-positives l)
    (length (filter positive? l)))