#lang racket

(provide make-adjacency-list small-cave?)

(define (read-input filename)
    (file->lines filename))

(define (input->adjacency-list lines)
    (foldl 
        (lambda (line ht) 
            (hash-set ht
                (first line)
                (append (hash-ref ht (first line) (lambda () empty)) (list (last line)))))
        #hash() 
        (split-lines lines)))

(define (split-lines lines) 
    (filter-not 
        (lambda (l) (or (string=? "start" (last l)) (string=? "end" (first l))))
        (apply append 
            (map (lambda (l) 
                (list 
                    (list (first l) (last l))
                    (list (last l) (first l))))
                (map (lambda (l) (string-split l "-")) lines)))))

(define (make-adjacency-list filename)
    (input->adjacency-list (read-input filename)))

(define (small-cave? s)
    (foldl (lambda (c v) (and v (char-lower-case? c))) #t (string->list s)))
