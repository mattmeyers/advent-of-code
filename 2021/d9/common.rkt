#lang racket

(provide
    get-input
    get-low-point-mask)

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines)
    (define line-len (string-length (car lines)))
        (append (map string->int-list lines)))

(define (get-input filename)
    (parse-input (read-input filename)))

(define (string->int-list s)
    (foldl (lambda (c lst) (append lst (list (string->number c)))) '() (string->chars s)))

(define (string->chars s)
    (drop-right (cdr (string-split s "")) 1))

(define (transpose mtx)
    (apply map list mtx))

(define (pad-right lst [padding +inf.0])
    (append lst (list padding)))

(define (find-rightward-mins lst)
    (if (empty? (cdr lst))
        '()
        (append
            (if (< (car lst) (cadr lst)) (list 1) (list 0))
            (find-rightward-mins (cdr lst)))))

(define (find-list-mins lst)
    (apply map * 
        (list
            (find-rightward-mins (pad-right lst))
            (reverse (find-rightward-mins (pad-right (reverse lst)))))))

(define (find-matrix-mins mtx)
    (map find-list-mins mtx))

(define (multiply-matrices as bs)
    (apply map * (list as bs)))

(define (get-low-point-mask mtx)
    (multiply-matrices 
        (flatten (find-matrix-mins mtx))
        (flatten (transpose (find-matrix-mins (transpose mtx))))))