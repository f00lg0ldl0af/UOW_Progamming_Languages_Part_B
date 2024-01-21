; Programming Languages, Dan Grossman
; Section 5: Avoiding Unnecessary Computations: Delay and Force

; [this is the code for two segments related to thunking and delay/force] 


; Key concept: Mutation & Thunking -> Lazy Evaluation (LE)
; LE: delaying computations you might not need.
;;    Make sure you do not do them more than once.

#lang racket

(provide (all-defined-out))

; this is a silly addition function that purposely runs slows for 
; demonstration purposes
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

; multiplies x and result of y-thunk, calling y-thunk x times
(define (my-mult x y-thunk) ;; assumes x is >= 0
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))
        
;; What if I just include the thunk as-is in function argument?
;; ============================================================
;(my-mult 0 (lambda () (slow-add 3 4)))
;(my-mult 1 (lambda () (slow-add 3 4)))
;(my-mult 2 (lambda () (slow-add 3 4)))

;; What if I precompute the expression (outside the thunk) in the thunk?
;; =====================================================================
;(my-mult 0 (let ([x (slow-add 3 4)]) (lambda () x)))
;(my-mult 1 (let ([x (slow-add 3 4)]) (lambda () x)))
;(my-mult 2 (let ([x (slow-add 3 4)]) (lambda () x)))

(define (my-delay2 some-thunk) ;create a promise (delay execution)
  (mcons #f some-thunk))

(define (my-force2 some-p) ;executes promise 
  (if (mcar some-p)
      (mcdr some-p)
      (begin (set-mcar! some-p #t)
             (set-mcdr! some-p ((mcdr some-p)))
             (mcdr some-p))))
;; ===================================================================
(define (my-delay th) 
  (mcons #f th)) ;; a one-of "type" we will update /in place/

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

;; What if I use a thunk (in second argument) with a promise? 
;; ==========================================================
(my-mult 0 (let ([x (my-delay (lambda () (slow-add 3 4)))]) ; x binds to a mutable pair (i.e., promise)
             (lambda () (my-force x))))

;(my-mult 1 (let ([x (my-delay (lambda () (slow-add 3 4)))]) (lambda () (my-force x))))
;(my-mult 2 (let ([x (my-delay (lambda () (slow-add 3 4)))]) (lambda () (my-force x))))
