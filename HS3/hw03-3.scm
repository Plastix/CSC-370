;; CSC 370 HS 3
;; Aidan Pieper
;; 1/25/17

; Load the Unit Tester
(load "../HS2/soln/1-soln.scm") 
(clear!)


(define true
  (lambda (a)
    (lambda (b)
      a)))

(define false
  (lambda (a)
    (lambda (b)
      b)))

(define if-ba 
  (lambda (test exp1 exp2)
    ((test exp1) exp2)))

;; Exercise 1
(define not-ba
  (lambda (a)
    (if-ba a
           false
           true)))


(define and-ba
  (lambda (a b)
    (if-ba a
           (if-ba b
                  true
                  false
                  )
           false)))

(define xor-ba
  (lambda (a b)
    (if-ba a
           (if-ba b
                  false
                  true
                  )
           (if-ba b
                  true
                  false))))

(add-batch-tests! "EX1" '(
                          (not-ba false) => true
                          (not-ba true) => false
                          (and-ba false false) => false
                          (and-ba true false) => false
                          (and-ba false true) => false
                          (and-ba true true) => true
                          (xor-ba false false) => false
                          (xor-ba true false) => true
                          (xor-ba false true) => true
                          (xor-ba true true) => false
                          ))

;; Exercise 2
(define ba->boolean
  (lambda (ba)
    (if-ba ba
           #t
           #f)))

(define boolean->ba
  (lambda (bool)
    (if bool
      true
      false)))

(add-batch-tests! "EX2" '(
                          (ba->boolean true) => #t
                          (ba->boolean false) => #f
                          (boolean->ba #t) => true
                          (boolean->ba #f) => false
                          ))
