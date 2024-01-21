; Programming Languages, Dan Grossman
; Section 5: Memoizaton

#lang racket

(provide (all-defined-out))

(define (fibonacci1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci1 (- x 1))   ;recursively every step, makes two more recursive calls 
         (fibonacci1 (- x 2)))))

;; ===================================================
(define (fibonacci2 x)
  (letrec ([f (lambda (acc1 acc2 y)
                (if (= y x)
                    (+ acc1 acc2)
                    (f (+ acc1 acc2) acc1 (+ y 1))))])
    ; body of letrec
    (if (or (= x 1) (= x 2))
        1
        (f 1 1 3))))

(define fibonacci3
  (letrec([memo null] ; list of pairs (arg . result) 
          [f (lambda (x)
               (let ([ans (assoc x memo)])
                 (if ans 
                     (cdr ans)
                     (let ([new-ans (if (or (= x 1) (= x 2))
                                        1
                                        (+ (f (- x 1)) ; 1st recursive call ends up filling 
                                                       ; the table with lots of numbers
                                           (f (- x 2))))]) ; 2nd one ends up finding what it
                                                           ; needs in table
                       ; make one recursive call, and then for next recursive call
                       ; everything needed is in table
                       
                       (begin 
                         (set! memo (cons (cons x new-ans) memo))
                         ; return fibonacci of the number 
                         new-ans))
                     )))])
    f))
