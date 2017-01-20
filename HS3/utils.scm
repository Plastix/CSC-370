;; CSC 370 HS 3
;; Aidan Pieper
;; 1/25/17

;; Utility functions

(define var-exp? symbol?)

(define lambda-exp?
  (lambda (exp)
    (and
      (list? exp)
      (= (length exp) 3)
      (equal? (car exp) 'lambda)
      (list? (cadr exp))
      (= (length (cadr exp)) 1)
      (var-exp? (caadr exp))
      )))

(define apply-exp?
  (lambda (exp)
    (and
      (list? exp)
      (= (length exp) 2) )))

(define lambda->body
  (lambda (l)
    (caddr l)))

(define lambda->param
  (lambda (l)
    (cadr l)))

(define remove-duplicates
  (lambda (ls)
    (fold-right 
      (lambda (head acc) 
        (cons head (remove head acc)))
      '()
      ls)))

