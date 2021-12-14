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

(define (split-pair key addition num frqs)
    (define split-key (string->list key))
    (define a (string->list addition))
    (foldl 
        (lambda (k fs) (hash-update fs (list->string k) (lambda (v) (+ v num)) 0)) 
        frqs 
        (list 
            (cons (car split-key) a) 
            (append a (cdr split-key)))))

(define (step rules frequencies)
    (foldl 
        (lambda (pair frqs) 
            (split-pair
                pair
                (hash-ref rules pair) 
                (hash-ref frequencies pair) 
                frqs)) 
        #hash() 
        (hash-keys frequencies)))

(define (run rules frequencies steps)
    (foldl (lambda (n frqs) (step rules frqs)) frequencies (range steps)))

(define (split-key-frqs frqs init-ht)
    (foldl 
        (lambda (k counts) 
            (hash-update 
                (hash-update 
                    counts 
                    (first (string->list k)) 
                    (lambda (v) 
                    (+ v (hash-ref frqs k))) 0) 
                (last (string->list k)) 
                (lambda (v) (+ v (hash-ref frqs k))) 
                0)) 
        init-ht
        (hash-keys frqs)))

(define (dedupe frqs)
    (foldl (lambda (k fs) (hash-set fs k (/ (hash-ref frqs k) 2))) #hash() (hash-keys frqs)))

(define (diff frequencies)
    (define sorted (sort (hash-values frequencies) <))
    (- (last sorted) (first sorted)))

(define (part-2 filename)
    (define lines (read-input filename))
    (define rules (parse-map (cddr lines)))
    (define frequencies (foldl (lambda (p ht) (hash-update ht p add1 0)) #hash() (get-windows (string->list (car lines)))))
    (define run-results (run rules frequencies 40))
    (define init-frq-map (hash
        (first (string->list (car lines)))  1
        (last (string->list (car lines)))  1
    ))
    (diff (dedupe (split-key-frqs run-results init-frq-map))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))

