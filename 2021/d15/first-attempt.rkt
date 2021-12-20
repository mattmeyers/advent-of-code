#lang racket

(struct vertex (pos weight) #:transparent)

(define width 10)
(define height 10)
(define size (* width height))

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines [i 0])
    (if (empty? lines)
        empty
        (append
            (parse-row (string->list (car lines)) i)
            (parse-input (cdr lines) (add1 i)))))

(define (parse-row row i [j 0])
    (if (empty? row)
        empty
        (append
            (list (cons (cons i j) (string->number (string (car row)))))
            (parse-row (cdr row) i (add1 j)))))

(define (get-input filename)
    (parse-input (read-input filename)))

(define (build-lookup lst)
    (foldl (λ (l ht) (hash-set ht (car l) (cdr l))) #hash() lst))

(define (origin? p)
    (and (zero? (car p)) (zero? (cdr p))))

(define (build-init-vertices lst)
    (foldl (λ (l ht) (hash-set ht (car l) (if (origin? (car l)) 0 +inf.0))) #hash() lst))

(define (min-pos vertices spt ps [min-dist +inf.0] [min-p (cons -1 -1)])
    (if (empty? ps)
        min-p
        (min-pos 
            vertices
            spt
            (cdr ps) 
            (if (not (hash-has-key? spt (car ps))) 
                (min min-dist (hash-ref vertices (car ps)))
                min-dist)
            (if 
                (and 
                    (not (hash-has-key? spt (car ps))) 
                    (< (hash-ref vertices (car ps)) min-dist)) 
                (car ps) 
                min-p))))

(define (surrounding-vertices v spt)
    (filter-not 
        (λ (val) (hash-has-key? spt val)) 
        (filter-not
            (λ (p) 
                (or 
                    (negative? (car p)) 
                    (negative? (cdr p))
                    (>= (car p) height)
                    (>= (cdr p) width)))
            (list
                (cons (sub1 (car v)) (cdr v))
                (cons (add1 (car v)) (cdr v))
                (cons (car v) (sub1 (cdr v)))
                (cons (car v) (add1 (cdr v)))))))

(define (update-vertices vertices ps cost lookup)
    (foldl (λ (p vs) (hash-update vs p (λ (v) (+ (hash-ref lookup p) cost)))) vertices ps))

(define (dijkstra vertices spt lookup [steps size])
    ; (print-spt spt)
    (if (= steps (length (hash-keys spt)))
        spt
        (dijkstra 
            (update-vertices 
                vertices 
                (surrounding-vertices (min-pos vertices spt (hash-keys vertices)) spt) 
                (hash-ref vertices (min-pos vertices spt (hash-keys vertices))) 
                lookup)
            (hash-set spt (min-pos vertices spt (hash-keys vertices)) (hash-ref vertices (min-pos vertices spt (hash-keys vertices))))
            lookup
            steps)))

(define (print-spt spt)
    (for-each 
        (λ (i) 
            (for-each 
                (λ (j) 
                    (if (= 9 j) 
                        (displayln (hash-ref spt (cons i j) "-"))
                        (display (hash-ref spt (cons i j) "-")))) 
                (range 10)))
        (range 10))
    (displayln ""))
    
(define (part-1 filename)
    (define input (get-input filename))
    (define lookup (build-lookup input))
    (define vertices (build-init-vertices input))
    (hash-ref (dijkstra vertices #hash() lookup) (cons 9 9)))

(part-1 "input.txt")