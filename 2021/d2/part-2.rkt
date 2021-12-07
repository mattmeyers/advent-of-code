#lang racket

(require racket/runtime-path)
(require (rename-in "common.rkt" [position base-position]))

(struct position base-position (aim) #:transparent)

(define (handle-pos-change del pos)
    (cond
        (
            (string=? (delta-direction del) "forward") 
            (position 
                (+ 
                    (position-vertical pos)
                    (* (position-aim pos) (delta-distance del)))
                (+ (position-horizontal pos) (delta-distance del))
                (position-aim pos)))
        (
            (string=? (delta-direction del) "up") 
            (position 
                (position-vertical pos)
                (position-horizontal pos)
                (- (position-aim pos) (delta-distance del))))
        (
            (string=? (delta-direction del) "down") 
            (position 
                (position-vertical pos)
                (position-horizontal pos)
                (+ (position-aim pos) (delta-distance del))))
        (else pos)))

(define (calculate-position ds)
    (foldl handle-pos-change (position 0 0 0) ds))

(define (part-2 filename)
    (position-product (calculate-position (build-deltas (read-input filename)))))

(provide part-2)

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-2 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-2 (path->string input-file)))
