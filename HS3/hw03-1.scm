;; CSC 370 HS 3
;; Aidan Pieper
;; 1/25/17

; Load the Unit Tester
(load "../HS2/hw2-1.scm") 

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

