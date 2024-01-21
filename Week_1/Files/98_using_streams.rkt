; Programming Languages, Dan Grossman
; Section 5: Using and Defining Streams

; [same code for segments on using streams and defining streams]

#lang racket

(provide (all-defined-out))

;; define some streams

;(define ones-really-bad (cons 1 ones-really-bad))
(define ones-bad (lambda () (cons 1 (ones-bad))))

(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

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

;; Practice 
;; ====================================================================================
(define (chk-stream str tester)
  (letrec (; with letrec, expressions are evaluated in the environment with all the bindings
           ; f binding 
           [f (lambda (str acc)
                ; body of f
                (let ([a-pair (str)]) ; get str from closure 
                  (if (tester (car a-pair)) 
                      acc
                      ; not possible with let or let* since let -> expressions are evaluated in environment before let-expression
                      ; let* -> expressions are evaluated in environment from previous bindings
                      (f (cdr a-pair) (+ acc 1)))))] 
           )
    ;; body of letrec
    (f str 1)))

(define (num-til-fn stream test-fn)
  ; stop at the nth element of stream when test-fn on nth-element is true, and return n
  ; need a tail-recursive fn
  (letrec
      ; bindings 
      ([rec-fn (lambda (stream acc)
                 ; rec-fn will return the acc if test-fn on first element of pair is true 
                     (if (test-fn (car (stream)))
                         acc
                         ;; go to next element of pair (i.e., stream)
                         (rec-fn (cdr (stream)) (+ acc 1))))]) ;; unnecessary re-calling of the thunk
    (rec-fn stream 1)))
    
    
(define (num-til-fn2 stream test-fn)
  (letrec([rec-fn (lambda (stream acc)
                    (let ([pr (stream)])
                      (if (test-fn (car pr))
                          acc
                          (rec-fn (cdr pr) (+ acc 1)))
                    ))]) 
    (rec-fn stream 1)))




