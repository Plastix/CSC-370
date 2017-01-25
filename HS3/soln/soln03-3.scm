;;(load "grader.ss")

;;(load "3.ss")

;; Here is an interface for Boolean arithmetic:

;; true = /true/
;; false = /false/
;; (and-ba /a/ /b/) = /a and b/
;; (not-ba /a/) = /not a/
;; (xor-ba /a/ /b/) = /a exclusive-or b/
;; (if-ba /test/ exp1 exp2) = exp1 if test is true, and exp2 if test
;; is false

;; Note the lack of parens around true and false. Consider the
;; following partial implementation of this interface:

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

;; 1. Implement the other three functions -- not, and, and xor -- with
;; respect to this representation of Boolean values.

(define not-ba
  (lambda (a)
    ((a false) true)))

(define and-ba
  (lambda (a b)
    ((a b) false)))

(define or-ba
  (lambda (a b)
    (not-ba (and-ba (not-ba a) (not-ba b)))))

(define xor-ba
  (lambda (a b)
    (or-ba (and-ba (not-ba a) b) (and-ba a (not-ba b)))))

;; (add-batch-tests! "Problem 3.2.1" '(
;; (not-ba false) => true
;; (not-ba true) => false
;; (and-ba true false) => false
;; (and-ba true true) => true
;; (xor-ba true true) => false
;; (xor-ba false true) => true
;; ))

;; 2. Write functions ba->boolean and boolean->ba that convert between
;; the above the functional encoding of Boolean values and Scheme
;; Booleans, and vice versa.

(define ba->boolean
  (lambda (a)
    ((a #t) #f)))

(define boolean->ba
  (lambda (b)
    (if b true false)))

;; (add-batch-tests! "Problem 3.2.2" '(
;; (ba->boolean true) => #t
;; (ba->boolean false) => #f
;; (boolean->ba #t) => true
;; (boolean->ba #f) => false
;; )



;; (run-all-tests!)
