;; CSC 370 HS 3
;; Aidan Pieper
;; 1/25/17

; Load the Unit Tester
(load "../HS2/soln/1-soln.scm") 
(clear!)

(define zero
  (lambda ()
    (lambda (f)
      (lambda (x) x))))

(define is-Zero?
  (lambda (n)
    (zero? (cnum->number n))))

(define cnum->number
  (lambda (cnum)
    ((cnum add1) 0)))

(define number->cnum
  (lambda (num)
    (letrec ([compose 
               (lambda (n f x)
                 (cond
                   [(zero? n) x]
                   [else (f (compose (sub1 n) f x))]))])
      (lambda (f)
        (lambda (x)
          (compose num f x))))))

(define successor
  (lambda (cnum)
    (number->cnum (add1 (cnum->number cnum)))))

(define predecessor
  (lambda (cnum)
    (number->cnum (sub1 (cnum->number cnum)))))

(add-batch-tests! "Int Interface" '(
                                    (((zero) add1) 0) => 0
                                    (is-Zero? (zero)) => #t
                                    (is-Zero? (lambda (f) (lambda (x) x))) => #t
                                    (is-Zero? (lambda (f) (lambda (x) (f x)))) => #f
                                    (is-Zero? (lambda (f) (lambda (x) (f (f x))))) => #f
                                    (is-Zero? (lambda (f) (lambda (x) (f (f (f x)))))) => #f
                                    (cnum->number (zero)) => 0
                                    (cnum->number (lambda (f) (lambda (x) x))) => 0
                                    (cnum->number (lambda (f) (lambda (x) (f x)))) => 1
                                    (cnum->number (lambda (f) (lambda (x) (f (f x))))) => 2
                                    (cnum->number (lambda (f) (lambda (x) (f (f (f x)))))) => 3
                                    (((number->cnum 0) add1) 0) => 0
                                    (((number->cnum 1) add1) 0) => 1
                                    (((number->cnum 2) add1) 0) => 2
                                    (((number->cnum 3) add1) 0) => 3
                                    (cnum->number (successor (zero))) => 1
                                    (cnum->number (successor (successor (zero)))) => 2
                                    (cnum->number (successor (successor (successor (zero))))) => 3
                                    (cnum->number (predecessor (successor (zero)))) => 0
                                    (cnum->number (predecessor (predecessor (successor (successor (zero)))))) => 0
                                     ))

(define false (zero))
(define true (successor (zero)))
(define false? is-Zero?) ;; Better name for booleans

(define and-ba
  (lambda (a b)
    (if-ba a
           (if-ba b
                  true
                  false)
           false)))

(define not-ba
  (lambda (a)
    (if (false? a)
      true
      false)))

(define xor-ba
  (lambda (a b)
    (if-ba a
           (if-ba b
                  false
                  true)
           (if-ba b
                  true
                  false))))

(define if-ba
  (lambda (test exp1 exp2)
    (if (false? test)
      exp2
      exp1)))

(add-batch-tests! "Boolean Interface" '(
                                        false => (zero)
                                        (is-Zero? false) => #t
                                        (cnum->number true) => 1
                                        (is-Zero? true) => #f
                                        (cnum->number false) => 0
                                        (not-ba true) => false
                                        (not-ba false) => true
                                        (not-ba (not-ba true)) => true
                                        (not-ba (not-ba false)) => false 
                                        (and-ba true true) => true
                                        (and-ba true false) => false
                                        (and-ba false true) => false
                                        (and-ba false false) => false
                                        (xor-ba true true) => false
                                        (xor-ba false true) => true
                                        (xor-ba true false) => true
                                        (xor-ba false false) => false
                                        ))
