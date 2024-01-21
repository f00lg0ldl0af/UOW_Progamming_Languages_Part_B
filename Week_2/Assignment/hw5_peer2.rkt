;; Programming Languages, Homework 5

;; Hi peer.
;; Do you think you're a good person (I'm not)
;; Do you think there's a life after death?
;; Do you think you'll go to heaven or hell when you die?
;; Please watch this video: https://www.youtube.com/watch?v=TCSUKIhjevo
;; God bless you!

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; Testing:
(require rackunit)

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; 1 a (1)
(define (racketlist->mupllist l)
  (if (null? l)
      (aunit)
      (apair (car l) (racketlist->mupllist (cdr l)))))

(check-equal? (racketlist->mupllist (cons 1 (cons 2 (cons "hi" null))))
              (apair 1 (apair 2 (apair "hi" (aunit)))))


;; 1 b (2)
(define (mupllist->racketlist l)
  (if (aunit? l)
      null
      (cons (apair-e1 l) (mupllist->racketlist (apair-e2 l)))))

(check-equal? (mupllist->racketlist (apair 1 (apair "hi" (aunit))))
              (cons 1 (cons "hi" null)))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; 2 (3)
;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL add applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) (aunit)]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]
               [e3 (ifgreater-e3 e)]
               [e4 (ifgreater-e4 e)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env e3 env)
                   (eval-under-env e4 env))
               (error "MUPL ifgreater applied to non-integer")))]
        [(mlet? e)
         (let ([menv (cons (cons (mlet-var e)
                                 (eval-under-env (mlet-e e) env))
                           env)])
           (eval-under-env (mlet-body e) menv))]
        [(call? e)
         (let ([cclosure (eval-under-env (call-funexp e) env)])
           (if (closure? cclosure)
               (let* ([cfun (closure-fun cclosure)]
                      [cfun-name (fun-nameopt cfun)]
                      [cfun-body (fun-body cfun)]
                      [ccenv (closure-env cclosure)]
                      [ccenv-with-arg (cons (cons (fun-formal cfun)
                                                  (eval-under-env (call-actual e) env))
                                            ccenv)])
                 (if (false? cfun-name)
                     (eval-under-env cfun-body ccenv-with-arg)
                     (eval-under-env cfun-body
                                     (cons (cons cfun-name cclosure) ccenv-with-arg))))
               (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([cpair (eval-under-env (fst-e e) env)])
           (if (apair? cpair)
               (apair-e1 cpair)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([cpair (eval-under-env (snd-e e) env)])
           (if (apair? cpair)
               (apair-e2 cpair)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; These are my tests:

;; ifgreater
;(check-equal? (eval-under-env (ifgreater (int 1) (int 2) (int 3) (int 4)) null)
;              (int 4))
;; call
;(check-equal? (eval-under-env (mlet "a" (int 5)
;                                    (add (int 3) (var "a")))
;                              null)
;              (int 8))
;; recursive fun (call, fun, closure)
;(check-equal? (eval-under-env
;               (mlet "endnum" (int 400)
;                     (mlet "myfun" (fun "myfun" "n"
;                                        (ifgreater (var "n")
;                                                   (int 99)
;                                                   (var "endnum")
;                                                   (call (var "myfun") (add (var "n")
;                                                                            (int 1)))))
;                           (mlet "endnum" (int 401)
;                                 (call (var "myfun") (int 5)))))
;               null)
;              (int 400))
;;; lambda fun
;(check-equal? (eval-under-env
;               (call (fun #f "n" (add (var "n") (int 1))) (int 2))
;               null)
;              (int 3))
;;; pair
;(check-equal? (eval-under-env
;               (apair (add (int 1) (int 1)) (add (int 1) (int 2)))
;               null)
;              (apair (int 2) (int 3)))
;;; fst snd
;(check-equal? (eval-under-env
;               (mlet "pair" (apair (int 1) (int 2))
;                     (add (fst (var "pair")) (snd (var "pair"))))
;               null)
;              (int 3))
;; isaunit
;(check-equal? (eval-under-env
;               (isaunit (int 1))
;               null)
;              (int 0))
;; aunit
;(check-equal? (eval-under-env (aunit)
;                              null)
;              (aunit))
;;; list summer, which checks aunits
;(check-equal? (eval-under-env
;               (mlet "mylist"
;                     (apair (add (int 1) (int 2))
;                                     (apair (int 4) (aunit)))
;                     (mlet "sumlist"
;                           (fun "sumlist" "l"
;                                (ifgreater (isaunit (var "l")) (int 0)
;                                           (int 0)
;                                           (add (fst (var "l"))
;                                                (call (var "sumlist")
;                                                      (snd (var "l"))))))
;                           (call (var "sumlist") (var "mylist"))))
;               null)
;              (int 7))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

; 3 a (4)
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;(check-equal? (ifaunit (aunit) (add (int 1) (int 1)) (int 2))
;              (ifgreater (isaunit (aunit)) (int 0) (add (int 1) (int 1)) (int 2)))
;(check-equal? (eval-exp (ifaunit (aunit) (int 1) (int 2)))
;              (int 1))

; 3 b (5)
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

;(check-equal? (eval-exp (mlet* (list (cons "a" (add (int 1) (int 1))) (cons "b" (int 2)))
;                               (add (var "a") (var "b"))))
;              (int 4))
;(check-equal? (eval-exp (mlet* null (int 1)))           
;              (int 1))

; 3 c (6)
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;(check-equal? (eval-exp (ifeq (add (int 1) (int 2)) (int 3) (int 10) (int 11)))
;              (int 10))
;(check-equal? (eval-exp (ifeq (add (int 1) (int 2)) (int 5) (int 10) (int 11)))
;              (int 11))

;; Problem 4

; 4 a (7)
(define mupl-map
  (fun "map" "f"
       (fun "fmap" "l"
            (ifgreater (isaunit (var "l")) (int 0)
                       (aunit)
                       (apair (call (var "f") (fst (var "l")))
                              (call (var "fmap") (snd (var "l"))))))))

;(check-equal? (eval-exp
;               (mlet "addone" (fun #f "n" (add (int 1) (var "n")))
;                     (mlet "mylist" (apair (int 1) (apair (int 2) (aunit)))
;                           (call (call mupl-map (var "addone")) (var "mylist")))))
;              (apair (int 2) (apair (int 3) (aunit))))

; 4 b (8)
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "n" (add (var "i") (var "n")))))))

;(check-equal? (eval-exp (call (call mupl-mapAddN (int 5))
;                              (apair (int 1) (apair (int 2) (aunit)))))
;              (apair (int 6) (apair (int 7) (aunit))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
