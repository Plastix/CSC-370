;; CSC 370 HS 4
;; Aidan Pieper
;; 2/1/17

;; Abstract syntax definition
;; Source code: EOPL pg 55
(define-datatype prefix-exp prefix-exp? 
                 (const-exp
                   (num integer?))
                 (diff-exp
                   (operand1 prefix-exp?)
                   (operand2 prefix-exp?)))

(define parse-prefix
  (lambda (exp)
    (car (parse-prefix* exp))))

;; Concrete-Exp -> Abstract-Exp
(define parse-prefix*
  (lambda (exp)
    (cond
      [(const-exp? exp) (cons (const-exp (car exp)) (cdr exp))]
      [(and (diff-exp? exp)
            (const-exp? (cdr exp))) 
       (let* ([op1 (parse-prefix* (cdr exp))]
              [op2 (parse-prefix* (cddr exp))])
         (cons (diff-exp (car op1) (car op2)) (cdr op2)))]
      [(and (diff-exp? exp)
            (diff-exp? (cdr exp)))
       (let* ([op1 (parse-prefix* (cdr exp))]
              [op2 (parse-prefix* (cdr op1))])
         (cons (diff-exp (car op1) (car op2)) (cdr op2)))]
      [else (report-invalid-concrete-syntax exp)])))

;; Exp -> Boolean
(define non-null-ls?
  (lambda (exp)
    (and
      (list? exp)
      (not (null? exp)))))

;; Concrete-Exp -> Boolean
(define const-exp?
  (lambda (exp)
    (and
      (non-null-ls? exp)
      (integer? (car exp)))))

;; Concrete-Exp -> Boolean
(define diff-exp?
  (lambda (exp)
    (and
      (non-null-ls? exp)
      (equal? (car exp) '-))))

;; - - 1 - 5 4 - 4 - 1 2
;; - - 0 1 4
;;
