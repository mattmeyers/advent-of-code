#lang racket

(define BOARD_SIZE 10)

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines)
    (list->vector (map list->vector (map parse-line lines))))

(define (parse-line line)
    (map (lambda (c) (string->number (string c))) (string->list line)))

(define (increase-energy mtx)
    (for-each 
        (lambda (i) 
            (for-each 
                (lambda (j) (matrix-add1 mtx i j))
                (range BOARD_SIZE)))
        (range BOARD_SIZE)))

(define (reset-energy mtx)
    (for-each 
        (lambda (i) 
            (for-each 
                (lambda (j) (if (negative? (matrix-ref mtx i j)) (matrix-set! mtx i j 0) '()))
                (range BOARD_SIZE)))
        (range BOARD_SIZE)))

(define (flashers? mtx)
    (foldl 
        (lambda (row outerBool) 
            (or outerBool (foldl (lambda (v innerBool) (or innerBool (> v 9))) #f (vector->list row))))
        #f 
        (vector->list mtx)))

(define (matrix-ref mtx i j)
    (vector-ref (vector-ref mtx i) j))

(define (matrix-add1 mtx i j)
    (vector-set! 
        (vector-ref mtx i) 
        j 
        (add1 (vector-ref (vector-ref mtx i) j))))

(define (matrix-set! mtx i j v)
    (vector-set! (vector-ref mtx i) j v))

(define (matrix->flashers mtx)
    (filter-not empty? 
        (apply append 
            (map (lambda (i) 
                (map (lambda (j) 
                    (if (> (matrix-ref mtx i j) 9) (cons i j) '())) 
                (range BOARD_SIZE))) 
            (range BOARD_SIZE)))))

(define (flash mtx flashers)
    (for-each (lambda (f) (scar-for-life mtx f)) flashers)
    (length flashers))

(define (scar-for-life mtx flasher)
    (define victims (flashing-victims flasher))
    ; (displayln victims)
    (for-each 
        (lambda (v) 
            (if (and (= (car flasher) (car v)) (= (cdr flasher) (cdr v))) 
                (matrix-set! mtx (car v) (cdr v) -inf.0)
                (matrix-add1 mtx (car v) (cdr v))))
        victims))

(define (flashing-victims flasher)
    (filter 
        valid-pos
        (list
            (cons (sub1 (car flasher)) (sub1 (cdr flasher)))
            (cons (car flasher) (sub1 (cdr flasher)))
            (cons (add1 (car flasher)) (sub1 (cdr flasher)))

            (cons (sub1 (car flasher)) (cdr flasher))
            (cons (car flasher) (cdr flasher))
            (cons (add1 (car flasher)) (cdr flasher))

            (cons (sub1 (car flasher)) (add1 (cdr flasher)))
            (cons (car flasher) (add1 (cdr flasher)))
            (cons (add1 (car flasher)) (add1 (cdr flasher))))))

(define (valid-pos pos)
    (and 
        (>= (car pos) 0)
        (< (car pos) BOARD_SIZE)
        (>= (cdr pos) 0)
        (< (cdr pos) BOARD_SIZE)))

(define (propagate mtx [num-flashes 0])
    (if (flashers? mtx)
        (propagate mtx (+ num-flashes (flash mtx (matrix->flashers mtx))))
        num-flashes))

(define (step mtx)
    ; (pretty-print-mtx mtx)
    (increase-energy mtx)
    (define flashes (propagate mtx))
    (reset-energy mtx)
    flashes)

(define (run mtx [steps 1])
    (foldl (lambda (n c) (+ c (step mtx))) 0 (range steps)))

(define (all-zero? mtx)
    (= 0 (foldl (lambda (row sum) (+ sum (foldl + 0 (vector->list row)))) 0 (vector->list mtx))))

(define (do-step mtx)
    (define foo (step mtx))
    (void))

(define (run2 mtx [step 0])
    (define zero? (all-zero? mtx))
    (do-step mtx)
    (if (boolean=? #t zero?)
        step
        (run2 mtx (add1 step))))

(define (debug-print-mtx mtx)
    (display-lines (vector->list mtx))
    (displayln ""))

(define (pretty-print-mtx mtx)
    (display-lines (map (lambda (row) (apply string-append (map number->string (vector->list row)))) (vector->list mtx)))
    ; (display-lines (vector->list mtx))
    (displayln ""))

(define input (parse-input (read-input "input.txt")))
; (print-mtx input)
; (increase-energy input)
; (print-mtx input)
; (increase-energy input)
; (print-mtx input)

; (flashers? input)
; (matrix->flashers input)
; (flash input (matrix->flashers input))
; (print-mtx input)

; (flashers? input)
; (matrix->flashers input)
; (flash input (matrix->flashers input))
; (print-mtx input)

; (flashers? input)
; (matrix->flashers input)
; (flash input (matrix->flashers input))
; (print-mtx input)

; (flashers? input)
; (matrix->flashers input)
; (flash input (matrix->flashers input))
; (print-mtx input)
(run2 input)