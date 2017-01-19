;; CSC 370 HS 3
;; Aidan Pieper
;; 1/25/17

; Load the Unit Tester
(load "../HS2/soln/1-soln.scm") 
(clear!)

;; Some Test Cases
(define lc1 'x)
(define lc2 'y)
(define lc3 '(lambda (x) x))
(define lc4 '(lambda (y) x))
(define lc5 '(x y))
(define lc6 '(x (y y)))
(define lc7 '(x (lambda (y) y)))
(define lc8 '(lambda (x) (x y)))
(define lc9 '(lambda (x) (lambda (y) x)))
(define lc10 ' ((lambda (x) x) (lambda (y) y)))

; Load utility procedures
(load "utils.scm")

(define get-lvars
  (lambda (exp)
    (cond
      [(var-exp? exp) (list exp)]
      [(lambda-exp? exp) (get-lvars (lambda->body exp))]
      [(apply-exp? exp) (append (get-lvars (car exp)) 
                                (get-lvars (cadr exp)))])))

(add-batch-tests! "EX1" '(
                          (get-lvars lc1) => '(x)
                          (get-lvars lc2) => '(y)
                          (get-lvars lc3) => '(x)
                          (get-lvars lc4) => '(x)
                          (get-lvars lc5) => '(x y)
                          (get-lvars lc6) => '(x y y)
                          (get-lvars lc7) => '(x y)
                          (get-lvars lc8) => '(x y)
                          (get-lvars lc9) => '(x)
                          (get-lvars lc10) => '(x y)
                          (get-lvars '(lambda (x) (y z))) => '(y z)
                          (get-lvars '(z (lambda (x) x))) => '(z x)
                          (get-lvars '((lambda (f)  (lambda (x) ((g (g x)) y))) z)) => '(g g x y z)
                          ))

;; TODO AVOID DUPLICATES
;; Call remove on recursive call before appending??
(define get-lparams
  (lambda (exp)
    (cond
      [(var-exp? exp) (list)]
      [(lambda-exp? exp) (append (lambda->param exp) 
                                 (get-lparams (lambda->body exp)))]
      [(apply-exp? exp) (append (get-lparams (car exp))
                                (get-lparams (cadr exp)))])))

(add-batch-tests! "EX2" '(
                          (get-lparams lc1) => '()
                          (get-lparams lc2) => '()
                          (get-lparams lc3) => '(x)
                          (get-lparams lc4) => '(y)
                          (get-lparams lc5) => '()
                          (get-lparams lc6) => '()
                          (get-lparams lc7) => '(y)
                          (get-lparams lc8) => '(x)
                          (get-lparams lc9) => '(x y)
                          (get-lparams lc10) => '(x y)
                          (get-lparams '(lambda (x) (y z))) => '(x)
                          (get-lparams '((lambda (f) (lambda (x) ((g (g x)) y))) z) ) => '(f x)
                          (get-lparams '(lambda (x) (lambda (y) (lambda (x) z)))) => '(x y x)
                          ))
(define replace-vars
  (lambda (exp)
    (letrec ([counter
               (lambda (n exp)
                 (cond
                   [(var-exp? exp) n]
                   [(lambda-exp? exp) (list 
                                        (car exp) 
                                        (cadr exp)
                                        (counter (+ 1 n) (lambda->body exp)))]
                   [(apply-exp? exp) (append (list (counter n (car exp))) 
                                       (list (counter n (cadr exp))))])
                 )])
      (counter 0 exp)))) 

;; TODO MORE TESTS!!
(add-batch-tests! "EX3" '(
                          (replace-vars '(f x)) > '(0 0)
                          (replace-vars '(lambda (x) y)) => '(lambda (x) 1)
                          (replace-vars '(lambda (a) (lambda (b) c))) => '(lambda (a) (lambda (b) 2))
                          (replace-vars '(a ((lambda (a) b) c))) => '(0  ((lambda (a) 1) 0))
                          (replace-vars '((lambda (z) (lambda (y) ((lambda (x) (x y)) (x z)))) z))
                                      => '((lambda (z) (lambda (y) ((lambda (x) (3 3)) (2 2)))) 0)
                          ))

(define free-vars
  (lambda (exp)
    (free exp '())))

(define free
  (lambda (exp formals-seen)
    (cond
      [(var-exp? exp) (cond
                        [(member exp formals-seen) (list)]
                        [else (list exp)])]
      [(lambda-exp? exp) (free (lambda->body exp) (cons (car (lambda->param exp)) formals-seen))]
      [(apply-exp? exp) (append (free (car exp) formals-seen) 
                              (free (cadr exp) formals-seen))])))

(add-batch-tests! "EX4" '(
                          (free-vars 'x) => '(x)
                          (free-vars '(x (y z))) => '(x y z)
                          (free-vars '((lambda (x) x) y)) => '(y)
                          (free-vars '((lambda (z) z) (lambda (x) x))) => '()
                          (free-vars '((lambda (f) (lambda (x)  (f x))) y)) => '(y)
                          (free-vars '(lambda (x) z)) => '(z)
                          (free-vars '(lambda (x) (lambda (y) ((lambda (z) (x y)) z)))) => '(z)
                          ))

(define bound-vars
  (lambda (exp)
            (bound exp '())))

(define bound
  (lambda (exp formals-seen)
    (cond
      [(var-exp? exp) (cond
                        [(member exp formals-seen) (list exp)]
                        [else (list)])]
      [(lambda-exp? exp) (bound (lambda->body exp) (cons (car (lambda->param exp)) formals-seen))]
      [(apply-exp? exp) (append (bound (car exp) formals-seen)
                                (bound (cadr exp) formals-seen))])))

(add-batch-tests! "EX5" '(
                          (bound-vars 'x)  =>  '()
                          (bound-vars '(x (y z))) => '()
                          (bound-vars '((lambda (x) x) y)) => '(x)
                          (bound-vars '((lambda (z) z) (lambda (x) x))) => '(z x)
                          (bound-vars '((lambda (f) (lambda (x)  (f x))) y)) => '(f x)
                          (bound-vars '(lambda (x) z)) => '()
                          (bound-vars '(lambda (x) (lambda (y) ((lambda (z) (x y)) z)))) => '(x y)
                          ))

