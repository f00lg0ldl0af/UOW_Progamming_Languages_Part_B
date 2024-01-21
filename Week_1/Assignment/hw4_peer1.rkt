
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;Problem 1
(define (sequence low high stride)
  (cond [(> low high) '()]
        [#t (append (list low) (sequence (+ low stride) high stride))]))

;Problem 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;Problem 3
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;Problem 4
(define (stream-for-n-steps s n)
  (cond [(= n 0) '()]
        [#t (let ([res (s)])
              (cons (car res) (stream-for-n-steps (cdr res) (- n 1))))]))

;Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons
                        (if (= (remainder x 5) 0)
                            (* x -1)
                            x)
                        (lambda () (f (+ x 1)))))])
  (lambda () (f 1))))

;Problem 6
(define dan-then-dog
  (letrec ([f (lambda (s) (cons s (lambda () (f (if (= s "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

;Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
                (let ([a (stream)])
                  (cons (cons 0 (car a)) (lambda () (f (cdr a))))))])
    (lambda () (f s))))

;Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons
                           (cons (list-nth-mod xs n) (list-nth-mod ys n))
                           (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(equal? n (vector-length vec)) #f]
                      [#t (let ([value (vector-ref vec n)])
                            (if (pair? value)
                                (if (equal? v (car value))
                                    value
                                    (f (+ n 1)))
                                (f (+ n 1))))]))])
    (f 0)))

;Problem 10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [index 0])
    (lambda (v)
      (let ([cache-c (vector-assoc v cache)])
        (if (not cache-c)
            (let ([ans (assoc v xs)])
              (if (not ans)
                  #f
                  (begin (vector-set! cache index ans)
                         (set! index (if (= n (+ index 1)) 0 (+ index 1)))
                         ans)))
            cache-c)))))
