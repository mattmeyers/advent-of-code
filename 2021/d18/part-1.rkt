#lang racket

(struct node (l r) #:transparent)
(struct leaf (value) #:transparent)

(struct explosion (l r) #:transparent)

(define (node->explosion n)
    (explosion (leaf-value (node-l n)) (leaf-value (node-r n))))

(define (concatenate l r)
    (node l r))

(define (stringify n)
    (match n
        [(leaf v) (number->string v)]
        [(node l r) (format "[~a,~a]" (stringify l) (stringify r))]))

(define (parse-string input)
    (let-values ([(tree input) (parse input)]) 
        (if (not (empty? input))
            (raise "invalid input")
            tree)))

(define (parse input)
    (if (empty? input)
        empty
        (cond
            [(equal? (car input) #\[) (parse-node (cdr input))]
            [(char-numeric? (car input)) (parse-leaf input)]
            [else (raise "unbalanced brackets")])))

(define (parse-node input)
    (let*-values ([(l input) (parse input)] 
                  [(r input) (parse (cdr input))]) ; the cdr strips the ,
        (values (node l r) (cdr input)))) ; The cdr strips the ]

(define (parse-leaf input)
    (values 
        (leaf (string->number (list->string (takef input char-numeric?))))
        (dropf input char-numeric?)))

(define (explodable? tree)
    (>= (max-depth tree) 5))

(define (max-depth n [m 0])
    (match n
        [(leaf _) m]
        [(node l r) (max (max-depth l (add1 m)) (max-depth r (add1 m)))]))

(define (explode n)
    (let-values ([(new-root _1 _2 _3) (do-explode n)]) new-root))

(define (do-explode n [depth 0])
    ; (display depth)
    ; (display " ")
    ; (displayln (stringify n))
    (cond
        [(and (>= depth 4) (node? n) (leaf? (node-l n)) (leaf? (node-r n))) (values #f (node->explosion n) #f #f)]
        [(node? n) (let*-values ([(new-left-node left-explosion l-left-shrapnel l-right-shrapnel) (do-explode (node-l n) (add1 depth))]
                                 [(new-right-node right-explosion r-left-shrapnel r-right-shrapnel) (do-explode (node-r n) (add1 depth))])
                        (printf "n: ~a\n" (stringify n))
                        (printf "new-left-node: ~a\n" new-left-node)
                        (printf "left-explosion: ~a\n" left-explosion)
                        (printf "new-right-node: ~a\n" new-right-node)
                        (printf "right-explosion: ~a\n" right-explosion)
                        (cond
                            [(explosion? left-explosion) (values 
                                                (node (leaf 0) (leaf (+ (leaf-value (node-r n)) (explosion-r left-explosion))))
                                                #t
                                                (explosion-l left-explosion)
                                                #f)]
                            [(explosion? right-explosion) (values 
                                                (node (leaf (+ (leaf-value (node-l n)) (explosion-l right-explosion))) (leaf 0))
                                                #t
                                                #f
                                                (explosion-r right-explosion))]
                            [(and left-explosion l-right-shrapnel) (let-values ([(new-node left-explosion l-right-shrapnel l-left-shrapnel) (apply-explosion-right (node-r n) l-right-shrapnel)]) (values (node new-left-node new-node) explosion l-left-shrapnel l-right-shrapnel))]
                            [(and left-explosion l-left-shrapnel) (values (node new-left-node new-right-node) left-explosion l-left-shrapnel l-right-shrapnel)]
                            [(and right-explosion r-left-shrapnel) (let-values ([(new-node right-explosion r-right-shrapnel r-left-shrapnel) (apply-explosion-left (node-l n) r-left-shrapnel)]) (values (node new-node new-right-node) explosion r-left-shrapnel r-right-shrapnel))]
                            [(and right-explosion r-right-shrapnel) (values (node new-left-node new-right-node) right-explosion r-left-shrapnel r-right-shrapnel)]
                            [else (values (node new-left-node new-right-node) (or left-explosion right-explosion) #f #f)]))]
        [else (values n #f #f #f)]))

(define (apply-explosion-right n exp-val)
    (match n
        [(leaf v) (values (if exp-val (leaf (+ v exp-val)) n) #f #f #f)]
        [(node l r) (let-values ([(new-node _1 _2 _3) (apply-explosion-right l exp-val)]) (values (node new-node r) _1 _2 _3))]))

(define (apply-explosion-left n exp-val)
    (match n
        [(leaf v) (values (if exp-val (leaf (+ v exp-val)) n) #f #f #f)]
        [(node l r) (let-values ([(new-node _1 _2 _3) (apply-explosion-left r exp-val)]) (values (node l new-node) _1 _2 _3))]))

(define (splittable? tree)
    (match tree
        [(leaf v) (>= v 10)]
        [(node l r) (or (splittable? l) (splittable? r))]))

(define (split n)
    (let-values ([(n _) (do-split n)]) n))

(define (do-split n [done #f])
    (match n
        [(leaf v) 
            (if (and (not done) (>= v 10))
                (values (split-leaf-value v) #t)
                (values n done))]
        [(node l r) 
            (let*-values ([(nl done) (do-split l done)]
                          [(nr done) (do-split r done)])
                (values (node nl nr) done))]))

(define (split-leaf-value v)
    (node (leaf (floor (/ v 2))) (leaf (ceiling (/ v 2)))))

(define (reduce tree)
    (cond
        [(explodable? tree) (reduce (explode tree))]
        [(splittable? tree) (reduce (split tree))]
        [else tree]))

(define (add-all lines)
    (foldl 
        (λ (t acc) (reduce (concatenate acc (parse-string (string->list t)))))
        (reduce (parse-string (string->list (car lines))))
        (cdr lines)))

(define (magnitude n)
    (match n
        [(leaf v) v]
        [(node l r) (+ (* 3 (magnitude l)) (* 2 (magnitude r)))]))

(define (part-1 input)
    (reduce (parse-string (string->list input))))

; (define-values (t e ls rs) (part-1 "[[[[[9,8],1],2],3],4]")) ; [[[[0,9],2],3],4]
; (define-values (t e ls rs) (part-1 "[7,[6,[5,[4,[3,2]]]]]")) ; [7,[6,[5,[7,0]]]]
; (define-values (t e ls rs) (part-1 "[[6,[5,[4,[3,2]]]],1]")) ; [[6,[5,[7,0]]],3]
; (define-values (t e ls rs) (part-1 "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")) ; [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
; (define t (part-1 "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")) ; [[3,[2,[8,0]]],[9,[5,[7,0]]]]
; (define t (part-1 "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")) ; [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
; (displayln (stringify t))

; (define lines (file->lines "test-input.txt"))
; (define t1 (reduce (parse-string (string->list (car lines)))))
; (stringify t1)
; (define t2 (reduce (parse-string (string->list (cadr lines)))))
; (stringify t2)
; (define t3 (concatenate t1 t2))
; (stringify t3)

(part-1 "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]")

; (max-depth (parse-string (string->list "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]")))