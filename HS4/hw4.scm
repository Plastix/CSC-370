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
      [else (eopl:error 'parse-prefix "Invalid prefix exp: ~s" exp)])))

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
                                                   (const-exp 2))))
                          ))

;; Exercise 2
;; Abstract-Exp -> Concrete-Exp
(define unparse-prefix
  (lambda (exp)
    (cases prefix-exp exp
           (const-exp (num) (list num))
           (diff-exp (rand1 rand2) 
                     (append (list '-) (unparse-prefix rand1)
                           (unparse-prefix rand2)))
           (else (eopl:error 
                   'unparse-prefix
                   "Invalid abstract expression: ~s"
                   exp)))))

(add-batch-tests! "EX2" '(
                          (unparse-prefix (const-exp 1)) => '(1) 
                          (unparse-prefix (diff-exp (const-exp 1) (const-exp 2))) 
                          => '(- 1 2) 
                          (unparse-prefix (diff-exp 
                                            (diff-exp (const-exp 1)
                                                      (const-exp 2))
                                            (const-exp 3))) 
                          => '(- - 1 2 3) 
                          (unparse-prefix (diff-exp (const-exp 3)
                                                    (diff-exp 
                                                      (const-exp 0)
                                                      (const-exp 1)))) 
                          => '(- 3 - 0 1) 
                          (unparse-prefix (diff-exp (const-exp 1)
                                                    (diff-exp (const-exp 2)
                                                              (diff-exp
                                                                (const-exp 3)
                                                                (const-exp 0)))) ) 
                          => '(- 1 - 2 - 3 0) 
                          (unparse-prefix (diff-exp
                                            (diff-exp
                                              (diff-exp (const-exp 2) (const-exp 4))
                                              (diff-exp (const-exp 1) (const-exp 0)))
                                            (const-exp 0)))
                          => '(- - - 2 4 - 1 0 0)   
                          (unparse-prefix (diff-exp
                                            (diff-exp (const-exp 8) (const-exp 9))
                                            (diff-exp (const-exp 1)
                                                      (diff-exp (const-exp 7)
                                                                (const-exp 2)))))
                          => '(- - 8 9 - 1 - 7 2) 
                          ))

;; Exercise 3
;; Abstract-Exp -> SchemeNum
(define eval-prefix
  (lambda (exp)
    (eval (cases prefix-exp exp
           (const-exp (num) num)
           (diff-exp (rand1 rand2) 
                     (list '- (eval-prefix rand1)
                           (eval-prefix rand2)))
           (else (eopl:error 'eval-prefix 
                             "Invalid abstract expression ~s"
                             exp))))))

(add-batch-tests! "EX3" '(
                          (eval-prefix (const-exp 1)) => 1
                          (eval-prefix (diff-exp (const-exp 1) (const-exp 2))) 
                          => -1 
                          (eval-prefix (diff-exp 
                                            (diff-exp (const-exp 1)
                                                      (const-exp 2))
                                            (const-exp 3))) 
                          => -4 
                          (eval-prefix (diff-exp (const-exp 3)
                                                    (diff-exp 
                                                      (const-exp 0)
                                                      (const-exp 1)))) 
                          => 4 
                          (eval-prefix (diff-exp (const-exp 1)
                                                    (diff-exp (const-exp 2)
                                                              (diff-exp
                                                                (const-exp 3)
                                                                (const-exp 0)))) ) 
                          => 2 
                          (eval-prefix (diff-exp
                                            (diff-exp
                                              (diff-exp (const-exp 2) (const-exp 4))
                                              (diff-exp (const-exp 1) (const-exp 0)))
                                            (const-exp 0)))
                          => -3
                          (eval-prefix (diff-exp
                                            (diff-exp (const-exp 8) (const-exp 9))
                                            (diff-exp (const-exp 1)
                                                      (diff-exp (const-exp 7)
                                                                (const-exp 2)))))
                          => 3
                          ))
