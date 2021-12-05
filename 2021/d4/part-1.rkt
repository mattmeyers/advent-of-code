#lang racket

(define board-size 5)

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines)
    (cons (parse-moves (car lines)) (parse-boards (cdr lines))))

(define (parse-moves line)
    (string-split line ","))

(define (parse-boards lines)
    (if (empty? lines)
        empty
        (append 
            (list (parse-board (take (cdr lines) board-size)))
            (parse-boards (list-tail lines (add1 board-size))))))

(define (parse-board lines)
    (define parsed-lines (map extract-digits lines))
    (map (lambda (l) (map new-cell l)) parsed-lines))

(define (extract-digits s)
    (regexp-split #rx" +" (string-trim s)))

(define (new-cell n)
    (cons n #f))

(define (cell-value cell)
    (car cell))

(define (called! cell)
    (cons (car cell) #t))

(define (called? cell)
    (cdr cell))

(define (mark-number n board)
    (map 
        (lambda (r) 
            (map 
                (lambda (c) 
                    (if (string=? n (cell-value c)) 
                        (called! c) 
                        c)) 
                r)) 
        board))

(define (winner? boards)
    (cond
        [(empty? boards) empty]
        [(bingo? (car boards)) (car boards)]
        [else (winner? (cdr boards))]))

(define (bingo? board)
    (or (row-bingo? board) (col-bingo? board)))

(define (row-bingo? board)
    (ormap (lambda (r) (andmap called? r)) board))

(define (col-bingo? board)
    (row-bingo? (transpose board)))

(define (transpose board)
  (apply map list board))

(define (play-round n boards)
    (map (lambda (b) (mark-number n b)) boards))

(define (play numbers boards)
    (define winner (winner? boards))
    (if (not (empty? winner))
        (cons (length numbers) winner)
        (play (cdr numbers) (play-round (car numbers) boards))))

(define (final-number numbers remaining)
    (last (drop-right numbers remaining)))

(define (flatten-board board)
    (if (empty? board) 
        empty 
        (append (car board) (flatten-board (cdr board)))))

(define (score board final-number)
    (*
        (string->number final-number)
        (foldr + 0
            (filter-map 
                (lambda (c) (if (cdr c) #f (string->number (car c)))) 
                (flatten-board board)))))

(define (print-board board) 
    (for-each displayln board))

(define (print-boards boards)
    (for-each (lambda (b) (print-board (append b (list '("---"))))) boards))

(define (part-1)
    (define input (parse-input (read-input "input.txt")))
    (define winner (play (car input) (cdr input)))
    (score (cdr winner) (final-number (car input) (car winner))))

(part-1)
