#lang racket

(require racket/runtime-path)

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines)
    (define line-len (string-length (car lines)))
        (append (map string->int-list lines)))

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

(define (filter-minima mask mtx)
    (if (empty? mask)
        '()
        (append 
            (if (= 1 (car mask)) (list (car mtx)) '())
            (filter-minima (cdr mask) (cdr mtx)))))

(define (calculate-risk lst)
    (foldl (lambda (n s) (+ s (add1 n))) 0 lst))

(define (part-1 filename)
    (define input (parse-input (read-input filename)))
    (define mask (multiply-matrices 
        (flatten (find-matrix-mins input))
        (flatten (transpose (find-matrix-mins (transpose input))))))
    (calculate-risk (filter-minima mask (flatten input))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))