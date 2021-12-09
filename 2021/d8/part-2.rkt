#lang racket

;; We define the digits in terms of the standardized 8. This allows us to quickly identify
;; digits later by sorting them alphabetically and comparing.
;;
;;                       aaaa 
;;                      b    c
;;                      b    c
;;                       dddd 
;;                      e    f
;;                      e    f
;;                       gggg 
(define ZERO "abcefg")
(define ONE "cf")
(define TWO "acdeg")
(define THREE "acdfg")
(define FOUR "bcdf")
(define FIVE "abdfg")
(define SIX "abdefg")
(define SEVEN "acf")
(define EIGHT "abcdefg")
(define NINE "abcdfg")

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines)
    (define split-lines (map (lambda (l) (string-split l " | ")) lines))
    (map (lambda (l) (list (string-split (first l)) (string-split (last l)))) split-lines))

(define (parse-left lines)
    (map 
        string-split 
            (map 
                (lambda (l) (car (string-split l " | "))) 
                lines)))

(define (string->chars s)
    (drop-right (cdr (string-split s "")) 1))

(define (chars->string cs)
    (foldr string-append "" cs))

(define (string-diff a b)
    (define longer (if (>= (string-length a) (string-length b)) a b))
    (define shorter (if (string=? longer a) b a))
    (foldl 
        (lambda (c s) (string-append s (if (string-contains? shorter c) "" c)))
        ""
        (string->chars longer)))
    
(define (string-intersection a b)
    (define longer (if (>= (string-length a) (string-length b)) a b))
    (define shorter (if (string=? longer a) b a))
    (foldl 
        (lambda (c s) (string-append s (if (string-contains? shorter c) c "")))
        ""
        (string->chars longer)))

(define (string-union a b)
    (define longer (if (>= (string-length a) (string-length b)) a b))
    (define shorter (if (string=? longer a) b a))
    (foldl 
        (lambda (c s) (string-append s (if (string-contains? s c) "" c)))
        shorter
        (string->chars longer)))

(define (string-sub a b)
    (foldl 
        (lambda (c s) (string-append s (if (string-contains? b c) "" c))) 
        ""
        (string->chars a)))

(define (contains-chars str cs)
    (andmap (lambda (c) (string-contains? str c)) cs))

(define (find-by-len lst n)
    (findf (lambda (v) (= (string-length v) n)) lst))

(define (get-code lst)
    (define one (find-by-len lst 2))
    (define seven (find-by-len lst 3))
    (define four (find-by-len lst 4))
    (define eight (find-by-len lst 7))
    (define top (string-diff (find-by-len lst 3) one))
    (define six (findf (lambda (v) (and (= (string-length v) 6) (not (contains-chars v (string->chars one))))) lst))
    (define bottom-right (string-intersection one six))
    (define top-right (string-diff one bottom-right))
    (define two (findf (lambda (v) (and (= (string-length v) 5) (contains-chars v (list top-right)))) lst))
    (define middle (string-intersection two (string-sub four one)))
    (define top-left (string-sub (string-sub four one) middle))
    (define three (findf (lambda (v) (and (= (string-length v) 5) (contains-chars v (string->chars one)))) lst))
    (define nine (findf (lambda (v) (and (= (string-length v) 6) (contains-chars v (string->chars three)))) lst))
    (define bottom-left (string-diff eight nine))
    (define bottom (string-sub eight (string-union (string-union four top) bottom-left)))
    (hash
        top "a"
        top-right "c"
        bottom-right "f"
        middle "d"
        top-left "b"
        bottom-left "e"
        bottom "g"))

(define (identify-number s code-map)
    (define std-str (chars->string (sort 
        (map (lambda (c) (hash-ref code-map c)) (string->chars s))
        string<?)))
    (cond
        [(string=? std-str ZERO) "0"]
        [(string=? std-str ONE) "1"]
        [(string=? std-str TWO) "2"]
        [(string=? std-str THREE) "3"]
        [(string=? std-str FOUR) "4"]
        [(string=? std-str FIVE) "5"]
        [(string=? std-str SIX) "6"]
        [(string=? std-str SEVEN) "7"]
        [(string=? std-str EIGHT) "8"]
        [(string=? std-str NINE) "9"]))

(define (decode codes code-map)
    (string->number (foldr string-append "" (map (lambda (c) (identify-number c code-map)) codes))))

(define (part-2)
    (define data (parse-input (read-input "input.txt")))
    (foldl + 0 (map (lambda (datum) (decode (last datum) (get-code (first datum)))) data)))

(part-2)

