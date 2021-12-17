#lang racket

(require racket/runtime-path)

(struct packet (version type data) #:transparent)

(define (read-input filename)
    (file->string filename))

(define hexmap #hash(
    (#\0 . "0000")
    (#\1 . "0001")
    (#\2 . "0010")
    (#\3 . "0011")
    (#\4 . "0100")
    (#\5 . "0101")
    (#\6 . "0110")
    (#\7 . "0111")
    (#\8 . "1000")
    (#\9 . "1001")
    (#\A . "1010")
    (#\B . "1011")
    (#\C . "1100")
    (#\D . "1101")
    (#\E . "1110")
    (#\F . "1111")
))

(define (hex->bin h)
    (apply string-append (map (λ (c) (hash-ref hexmap c)) (string->list h))))

(define (bin->decimal b)
    (string->number (string-append "#b" b)))

(define (get-input filename)
    (string->list (hex->bin (read-input filename))))

(define (parse-version binstr)
    (define v (list->string (take binstr 3)))
    (match-define (list t d b) (parse-type (drop binstr 3)))
    (list v t d b))

(define (parse-type binstr)
    (define t (list->string (take binstr 3)))
    (cond
        [(string=? "100" t) 
            (match-let 
                ([(list d b) (parse-literal (drop binstr 3))])
                (list t d b))]
        [else (match-let
            ([(list d b) (parse-operator (drop binstr 3))])
            (list t d b))]))

(define (parse-literal binstr)
    (if (equal? #\0 (car binstr))
        (list 
            (list->string (take (cdr binstr) 4))
            (drop binstr 5))
        (match-let 
            ([(list s b) (parse-literal (drop binstr 5))])       
            (list 
                (string-append (list->string (take (cdr binstr) 4)) s)
                b))))

(define (trim-leading-zeroes binstr)
    (if (or (empty? binstr) (equal? #\1 (first binstr)))
        binstr
        (trim-leading-zeroes (cdr binstr))))

(define (parse-operator binstr)
    (if (equal? #\0 (car binstr))
        (let ([binlen (bin->decimal (list->string (take (cdr binstr) 15)))])
            (list 
                (parse-packets (take (drop binstr 16) binlen)) 
                (drop binstr (+ 16 binlen))))
        (let 
            ([ps (parse-n-packets (bin->decimal (list->string (take (cdr binstr) 11))) (drop binstr 12))]) 
            (list (drop-right ps 1) (last ps)))))

(define (parse-n-packets n binstr)
    (if (zero? n)
        (list binstr)
        (match-let 
            ([(list v t d b) (parse-version binstr)])
            (append (list (packet v t d)) (parse-n-packets (sub1 n) b)))))

(define (parse-packets binstr)
    (if (empty? (trim-leading-zeroes binstr))
        empty
        (match-let 
            ([(list v t d b) (parse-version binstr)])
            (cons (packet v t d) (parse-packets b)))))

(define (execute node)
    (cond
        [(string=? "000" (packet-type node)) 
            (foldl (λ (n acc) (+ acc (execute n))) 0 (packet-data node))]
        [(string=? "001" (packet-type node)) 
            (foldl (λ (n acc) (* acc (execute n))) 1 (packet-data node))]
        [(string=? "010" (packet-type node)) 
            (inexact->exact (foldl (λ (n acc) (min (execute n) acc)) +inf.0 (packet-data node)))]
        [(string=? "011" (packet-type node)) 
            (inexact->exact (foldl (λ (n acc) (max (execute n) acc)) -inf.0 (packet-data node)))]
        [(string=? "100" (packet-type node)) (bin->decimal (packet-data node))]
        [(string=? "101" (packet-type node))
            (let ([d (packet-data node)]) (if (> (execute (first d)) (execute (last d))) 1 0))]
        [(string=? "110" (packet-type node)) 
            (let ([d (packet-data node)]) (if (< (execute (first d)) (execute (last d))) 1 0))]
        [(string=? "111" (packet-type node)) 
            (let ([d (packet-data node)]) (if (= (execute (first d)) (execute (last d))) 1 0))]))

(define (part-2 filename)
    (execute (car (parse-packets (get-input filename)))))

(define-runtime-path test-input-file-1 "test-input-2-1.txt")
(define-runtime-path test-input-file-2 "test-input-2-2.txt")
(define-runtime-path test-input-file-3 "test-input-2-3.txt")
(define-runtime-path test-input-file-4 "test-input-2-4.txt")
(define-runtime-path test-input-file-5 "test-input-2-5.txt")
(define-runtime-path test-input-file-6 "test-input-2-6.txt")
(define-runtime-path test-input-file-7 "test-input-2-7.txt")
(define-runtime-path test-input-file-8 "test-input-2-8.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input 1: ~a~%" (part-2 (path->string test-input-file-1)))
(printf "Test Input 2: ~a~%" (part-2 (path->string test-input-file-2)))
(printf "Test Input 3: ~a~%" (part-2 (path->string test-input-file-3)))
(printf "Test Input 4: ~a~%" (part-2 (path->string test-input-file-4)))
(printf "Test Input 5: ~a~%" (part-2 (path->string test-input-file-5)))
(printf "Test Input 6: ~a~%" (part-2 (path->string test-input-file-6)))
(printf "Test Input 7: ~a~%" (part-2 (path->string test-input-file-7)))
(printf "Test Input 8: ~a~%" (part-2 (path->string test-input-file-8)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))