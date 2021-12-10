#lang racket

(require racket/runtime-path)

(define (read-input filename)
    (file->lines filename))

(define (parse-input lines)
    (define split-lines (map (lambda (line) (string-split line " | ")) lines))
    (map 
        (lambda (line) (string-split (last line)))
        split-lines))

(define (count-unique-entries lst)
    (foldl (lambda (v c) (+ c (if (member (string-length v) (list 2 3 4 7)) 1 0))) 0 lst))

(define (part-1 filename)
    (count-unique-entries (flatten(parse-input (read-input filename)))))

(define-runtime-path test-input-file "test-input.txt")
(define-runtime-path input-file "input.txt")
(printf "Test Input: ~a~%" (part-1 (path->string test-input-file)))
(printf "Full Input: ~a~%" (part-1 (path->string input-file)))