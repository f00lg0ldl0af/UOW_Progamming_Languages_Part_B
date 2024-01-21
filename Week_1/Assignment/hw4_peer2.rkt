#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null
  )
)


(define (string-append-map xs suffix)
  (map (lambda (x)(string-append x suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(null? xs) (error "list-nth-mod: empty list")]
        [(> 0 n) (error "list-nth-mod: negative number")]
        [#t (car (list-tail xs (remainder n (length xs))))]
  )
)


(define (stream-for-n-steps s n)
  (if (= n 0) '()
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))
  )
)


(define funny-number-stream
  (letrec (
           [f (lambda (x) (if (= 0 (remainder x 5)) (cons (* -1 x) (lambda () (f (+ 1 x)))) (cons x (lambda () (f (+ 1 x))))))]
           )
    (lambda () (f 1))
    )
  )


(define dan-then-dog
  (letrec (
           [f (lambda (flag)
                (if flag
                    (cons "dan.jpg" (lambda () (f (not flag))))
                    (cons "dog.jpg" (lambda () (f (not flag))))))]
           )
   (lambda () (f #t))
  )
)


(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s))) (lambda () (f s))))])
    (lambda () (f s))))


(define (cycle-lists xs ys)
  (letrec (
           [f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ 1 n)))))]
           )
    (lambda () (f 0))
    )
  )


(define (vector-assoc v vec)
  (letrec (
           [f (lambda (i v vec) (cond
                                 [(equal? i (vector-length vec)) #f]
                                 [(not (pair? (vector-ref vec i))) (f (+ 1 i) v vec)]
                                 [(equal? v (car (vector-ref vec i))) (vector-ref vec i)]
                                 [#t (f (+ 1 i) v vec)]
                                 )
                )
              ]
           )
    (f 0 v vec)
    )
  )
