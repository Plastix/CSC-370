;; CSC 370 HS 4
;; Aidan Pieper
;; 2/1/17

; Load the Unit Tester
(load "unit_tester.scm") 
(clear!)

;; Abstract syntax definition
;; Source code: EOPL pg 55
(define-datatype prefix-exp prefix-exp? 
                 (const-exp
                   (num integer?))
                 (diff-exp
                   (operand1 prefix-exp?)
                   (operand2 prefix-exp?)))
;; Exercise 1
;; Concrete-Exp -> Abstract-Exp
(define parse-prefix
  (lambda (exp)
    (car (parse-prefix* exp))))

;; Parses a prefix expression and returns a list
;; with the prefix expression as the car of the list
;; and the remaining elements as the cdr
;; A valid abstract expression will be returned as:
;; (Abstract-Exp)
;; denoting that there were no remaining elements
(define parse-prefix*
  (lambda (exp)
    (cond
      [(const-exp? exp) 
       (cons (const-exp (car exp)) (cdr exp))]
      [(diff-exp? exp)
       (let* ([rand1 (parse-prefix* (cdr exp))]
              [rand2 (parse-prefix* (cdr rand1))])
         (cons (diff-exp (car rand1) (car rand2)) 
               (cdr rand2)))]
      [else (report-invalid-concrete-syntax)])))

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

;; () -> Error
(define report-invalid-concrete-syntax
  (lambda ()
    (eopl:error parse-prefix "Invalid prefix exp!")))


(add-batch-tests! "EX1" '(
                          (parse-prefix '(1)) => (const-exp 1)
                          (parse-prefix '(- 1 2)) 
                          => (diff-exp (const-exp 1) (const-exp 2))
                          (parse-prefix '(- - 1 2 3)) 
                          => (diff-exp (diff-exp (const-exp 1)
                                                 (const-exp 2))
                                       (const-exp 3))
                          (parse-prefix '(- 3 - 0 1)) 
                          => (diff-exp (const-exp 3)
                                       (diff-exp 
                                         (const-exp 0)
                                         (const-exp 1)))
                          (parse-prefix '(- 1 - 2 - 3 0)) 
                          => (diff-exp (const-exp 1)
                                       (diff-exp (const-exp 2)
                                                 (diff-exp
                                                    (const-exp 3)
                                                    (const-exp 0))))
                          (parse-prefix '(- - - 2 4 - 1 0 0))
                          => (diff-exp
                               (diff-exp
                                 (diff-exp (const-exp 2) (const-exp 4))
                                 (diff-exp (const-exp 1) (const-exp 0)))
                               (const-exp 0))
                          (parse-prefix '(- - 8 9 - 1 - 7 2))
                          => (diff-exp
                               (diff-exp (const-exp 8) (const-exp 9))
                               (diff-exp (const-exp 1)
                                         (diff-exp (const-exp 7)
                                                   (const-exp 2))))))



