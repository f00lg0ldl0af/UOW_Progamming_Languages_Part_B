;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

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
(define (racketlist->mupllist racketlist)
  (if (null? racketlist) (aunit)
      (apair (car racketlist) (racketlist->mupllist (cdr racketlist)))))

(define (mupllist->racketlist mupllist)
  (if (equal? mupllist (aunit)) null
      (append (list (apair-e1 mupllist)) (mupllist->racketlist (apair-e2 mupllist)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
      
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e)
         (if (and (string? (fun-formal e)) (or (equal? (fun-nameopt e) #f) (string? (fun-nameopt e))))
             (closure env e)
             (error "MUPL fun-formal or fun-nameopt applied to non-string"))]
         ;(cond [(string? (fun-nameopt e)) (closure (append env (list (cons (fun-nameopt e) e))) e)]
              ; [(equal? (fun-nameopt e) #f) (closure env e)]
               ;[#t (error "MUPL fun nameopt is not #f and applied to non-string")])
         ;(error "MUPL fun formal applied to non-string"))]
        [(call? e)
         (let ([f (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? f)
               (letrec ([fn-name (fun-nameopt(closure-fun f))]
                      [f-env (if (string? fn-name)
                                 (list (cons fn-name f))
                                 null)]
                      [clos-env (closure-env f)]
                      [arg-env (list (cons (fun-formal(closure-fun f)) arg))]
                      [new-env (append f-env arg-env clos-env)])
                 (eval-under-env (fun-body (closure-fun f)) new-env))
               (error "MUPL call-funexp applied to a non-closure")))]
         ;(let ([v0 (eval-under-env (call-funexp e) env)])
           ;(if (closure? v0)
               ;(let* ([v1 (eval-under-env (call-actual e) env)]
                      ;[new-env (append (closure-env v0) (list (cons (fun-formal(closure-fun v0)) v1)))])
                 ;(eval-under-env (fun-body (closure-fun v0)) new-env))
               ;(error "MUPL call-funexp applied to a non-closure")))]
        [(apair? e) e
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
             (apair-e1 v1)
             (error "MUPL fst applied to a non-apair")))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
           (error "MUPL snd applied to a non-apair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (if (equal? (aunit) (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
         
        [(mlet? e)
         (let ([v1 (eval-under-env (mlet-e e) env)]
               [var1 (mlet-var e)])
           (if (string? var1)
           (eval-under-env (mlet-body e) (append env (list (cons var1 v1))))
           (error "MUPL mlet var applied to non-str")))]
        [(closure? e) e]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([var (car(car lstlst))]
            [e (cdr(car lstlst))])
        (mlet* (cdr lstlst)
               (mlet var e e2)))))
      

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x")
                               e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "fn"
       (fun "mp" "lst"
            (ifaunit (var "lst") (aunit)
                     (apair (call (var "fn") (fst (var "lst")))
                            (call (var "mp") (snd (var "lst"))))
                     ))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
       (fun #f "N"
            (call (var "map")
                  (fun #f "x"
                       (add (var "x") (var "N")))))))

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