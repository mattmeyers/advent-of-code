#lang racket

(require racket/runtime-path)

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

(define (update-visited visited v)
    (if (small-cave? v) 
        (hash-set visited v (add1 (hash-ref visited v 0))) 
        visited))

(define (can-visit-again? visited)
    (define values (hash-values visited))
    (= (length values) (foldl + 0 values)))

(define (get-children al curr visited)
    (filter
        (lambda (n) 
            (or 
                (zero? (hash-ref visited n 0))
                (and (= (hash-ref visited n) 1) (can-visit-again? visited))))
        (hash-ref al curr)))

(define (build-routes al visited curr)
    (define new-visited (update-visited visited curr))
    (if (string=? curr "end")
        (list curr)
        (append 
            (list curr) 
            (map 
                (lambda (n) (build-routes al new-visited n)) 
                (get-children al curr new-visited)))))

(define (count-routes routes)
    (if (empty? (cdr routes))
        (if (string=? "end" (car routes)) 1 0)
        (foldl (lambda (r c) (+ c (count-routes r))) 0 (cdr routes))))

(define (part-2 filename)
    (define al (make-adjacency-list filename))
    (count-routes(build-routes al #hash() "start")))

(define-runtime-path test-input-1-file "test-input-1.txt")
(define-runtime-path test-input-2-file "test-input-2.txt")
(define-runtime-path test-input-3-file "test-input-3.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input 1: ~a~%" (part-2 (path->string test-input-1-file)))
(printf "Test Input 2: ~a~%" (part-2 (path->string test-input-2-file)))
(printf "Test Input 3: ~a~%" (part-2 (path->string test-input-3-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))