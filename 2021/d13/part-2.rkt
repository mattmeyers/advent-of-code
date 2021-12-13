#lang racket

(define (read-input filename)
    (file->string filename))

(define (split-input input)
    (string-split input "\n\n"))

(define (parse-grid input)
    (map 
        (lambda (line) 
            (cons 
                (string->number (first line)) 
                (string->number (last line))))
        (map 
            (lambda (line) (string-split line ",")) 
            (string-split input "\n"))))

(define (get-input filename)
    (list->set (parse-grid (car (split-input (read-input filename))))))

(define (fold-up line grid)
    (define below (filter (lambda (point) (> (cdr point) line)) (set->list grid)))
    (define above (filter (lambda (point) (< (cdr point) line)) (set->list grid)))
    (foldl
        (lambda (point st) (set-add st point))
        (list->set above)
        (map
            (lambda (point) 
                (cons 
                    (car point) 
                    (- (cdr point) (* 2 (abs (- (cdr point) line)))))) 
            below)))

(define (fold-left line grid)
    (define right (filter (lambda (point) (> (car point) line)) (set->list grid)))
    (define left (filter (lambda (point) (< (car point) line)) (set->list grid)))
    (foldl 
        (lambda (point st) (set-add st point))
        (list->set left) 
        (map
            (lambda (point) 
                (cons 
                    (- (car point) (* 2 (abs (- (car point) line)))) 
                    (cdr point))) 
            right)))



(define code (fold-up 6 (fold-up 13 (fold-up 27 (fold-left 40 (fold-up 55 (fold-left 81 (fold-up 111 (fold-left 163 (fold-up 223 (fold-left 327 (fold-up 447 (fold-left 655 (get-input "input.txt"))))))))))))))


(define canvas (make-vector 6))
(for-each (lambda (n) (vector-set! canvas n (make-vector 40 "."))) (range 6))
(for-each 
    (lambda (point) 
        (vector-set! (vector-ref canvas (cdr point)) (car point) "#")) 
    (set->list code))
(for-each displayln (vector->list canvas))