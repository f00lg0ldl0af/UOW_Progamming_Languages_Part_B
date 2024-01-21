; Programming Languages, Dan Grossman
; Section 5: Using and Defining Streams

; [same code for segments on using streams and defining streams]

#lang racket

(provide (all-defined-out))

;; define some streams
; streams are not infinitely long, they are things that give you additional values when required (each time you call the thunk)

;(define ones-really-bad (cons 1 ones-really-bad)) ; Error: "cannot reference an identifier before its definition"
(define ones-bad (lambda () (cons 1 (ones-bad)))) ; infinitely long loop because with (ones-bad), it puts the result of calling that thunk, and in the cdr of that thunk,
                                                  ; is the result of calling that thunk, which its cdr is the ... and so on
                                                  ; "Interactions disabled; out of memory"

(define ones (lambda () (cons 1 ones)))

(define (fn x) (cons x (lambda () (fn (+ x 1)))))

(define nats-bad (lambda () (fn 1)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

;; helper function to abstract away the repetitive parts
(define (stream-maker fn arg)
  (letrec ([f (lambda (x) 
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define nats2  (stream-maker + 1))
(define powers2 (stream-maker * 2))

;; code that uses streams

(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))

(define four (number-until powers-of-two (lambda (x) (= x 16))))
