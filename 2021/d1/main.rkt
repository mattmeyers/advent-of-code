#lang typed/racket

(provide part-1 part-2)

(: read-input (-> String (Listof Integer)))
(define (read-input filename)
    (file->list filename))

(: sum (-> (Listof Integer) Integer))
(define (sum l)
    (foldr + 0 l))

(: windowify (-> (Listof Integer) (Listof Integer)))
(define (windowify l)
    (if (= (length l) 3)
        (list (foldr + 0 l))
        (append (list (sum (take l 3))) (windowify (cdr l)))))

(: zip-diff (-> (Listof Integer) (Listof Integer)))
(define (zip-diff l)
    (map - (cdr l) (drop-right l 1)))

(: count-positives (-> (Listof Integer) Integer))
(define (count-positives l)
    (length (filter positive? l)))

(: part-1 (-> Integer))
(define (part-1)
     (count-positives (zip-diff (read-input "input.txt" ))))

(define (part-2)
    (count-positives (zip-diff (windowify (read-input "input.txt")))))

(part-1)
(part-2)