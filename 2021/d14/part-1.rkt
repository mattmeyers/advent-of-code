#lang racket

(require racket/runtime-path)

(define (read-input filename)
    (file->lines filename))

(define (parse-template str)
    (get-windows (string->list str)))

(define (parse-map lines)
    (foldl 
        (lambda (line ht) (hash-set ht (first line) (last line)))
        #hash() 
        (map (lambda (line) (string-split line " -> ")) lines)))

(define (get-windows lst)
    (if (empty? (cdr lst))
        empty
        (cons
            (list->string (list (car lst) (cadr lst)))
            (get-windows (cdr lst)))))

(define (get-additions rules template)
    (map (lambda (pair) (hash-ref rules pair)) (get-windows (string->list template))))

(define (interlace base additions)
    (if (empty? additions)
        base
        (append (list (car base) (car additions)) (interlace (cdr base) (cdr additions)))))

(define (step rules template)
    (list->string 
        (interlace 
            (string->list template) 
            (flatten (map string->list (get-additions rules template))))))

(define (run rules template steps)
    (foldl (lambda (n s) (step rules s)) template (range steps)))

(define (count-frequency template [ht #hash()])
    (if (empty? template)
        ht
        (count-frequency 
            (cdr template)
            (hash-update ht (car template) add1 0))))

(define (diff frequencies)
    (define sorted (sort (hash-values frequencies) <))
    (- (last sorted) (first sorted)))


(define (part-1 filename)
    (define lines (read-input filename))
    (define rules (parse-map (cddr lines)))
    (diff (count-frequency (string->list (run rules (car lines) 10)))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))
