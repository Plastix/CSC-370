;;(load "grader.ss")

;;(load "3.ss")


;; (zero) = /0/
;; (is-Zero? /n/) = #t if /n = 0/, #f otherwise
;; (successor /n/) = /n+1/
;; (predecessor /n+1/) = /n/    ;;  (n >= 0)

;; 1.  Write an implementation of this interface, i.e., the four
;; functions above, that encodes non-negative integers as strings of
;; 0s and 1s.  In particular,

;; /0/  =  '(0)
;; /1/  =  '(1)
;; /2/  =  '(1 0)
;; /3/  =  '(1 1)
;; /4/  =  '(1 0 0)
;; /15/ =  '(1 1 1 1)
;; /16/ =  '(1 0 0 0 0)

(define zero
  (lambda ()
    '(0)))

(define is-Zero? 
  (lambda (a)
    (equal? a (zero))))


(define successor 
  (lambda (a)
    (reverse (successor* (reverse a)))))

(define successor*
  (lambda (a)
    (cond 
     [(null? a) (list 1)]
     [(= (car a) 0) (cons 1 (cdr a))]
     [else (cons 0 (successor* (cdr a)))])))
  
(define predecessor
  (lambda (a)
    (cond 
     [(is-Zero? a) (zero)]
     [(equal? a '(1)) (zero)]
     [else (reverse (predecessor* (reverse a)))])))

(define predecessor*
  (lambda (a) 
    (cond
     ;; We don't have a null? check because numbers are not zero
     ;; padded at front
     [(= (car a) 1)
      (if (null? (cdr a)) 
	  '() 
	  (cons 0 (cdr a)))] 
     [else (cons 1 (predecessor* (cdr a)))])))


;; (add-batch-tests! "Problem 3.1.1" '(
;; (zero) => '(0)
;; (is-Zero? '(0)) => #t
;; (is-Zero? '(1 0 0)) => #f
;; (successor '(0)) => '(1)
;; (successor '(1 1 1 1 1 1)) => '(1 0 0 0 0 0 0)
;; (predecessor '(1)) => '(0)
;; (predecessor '(1 0 0 0 0)) => '(1 1 1 1)
;; (predecessor '(1 0 1 1 1)) => '(1 0 1 1 0)
;; ))

;; 2.  Write functions binary->number and number->binary that convert
;; between the above the binary encoding of non-negative integers and
;; Scheme integers, and vice versa.

(define binary->number
  (lambda (a)
    (fold-left (lambda (acc head) (+ (* 2 acc) head)) 0 a)))

(define number->binary
  (lambda (n)
    (reverse (number->binary* n))))

(define number->binary*
  (lambda (n)
    (cond
     [(= n 0) (zero)]
     [(= n 1) '(1)]
     [else (let
	       ([m (mod n 2)])
	     (cons m (number->binary* (/ (- n m) 2))))])))


;; (add-batch-tests! "Problem 3.1.2" '(
;; (binary->number '(0)) => 0
;; (binary->number '(1 0 1 0 1)) => 21
;; (number->binary 32) => '(1 0 0 0 0 0)
;; (number->binary 0) => '(0)
;; ))
  
;; 3.  Using the interface above write representation-independent
;; implementations of the functions below.

;; (equals? /a/ /b/) = /a equals b?/
;; (less-than? /a/ /b/) = /a less than b?/
;; (sum /a/ /b/) = /a+b/
;; (prod /a/ /b/) = /a*b/

(define equals?
  (lambda (a b)
    (cond
     [(and (is-Zero? a) (is-Zero? b)) #t]
     [(or (is-Zero? a) (is-Zero? b)) #f]
     [else (equals? (predecessor a) (predecessor b))])))
    
(define less-than?
  (lambda (a b)
    (cond
     [(is-Zero? b) #f]
     [(is-Zero? a) #t]
     [else (less-than? (predecessor a) (predecessor b))])))

;; We did this one in class.
(define sum
  (lambda (a b)
    (cond
     [(is-Zero? a) b]
     [else (sum (predecessor a) (successor b))])))

;; We did this one in class.
(define prod
  (lambda (a b)
    (cond
     [(is-Zero? a) (zero)]
     [else (sum b (prod (predecessor a) b))])))

;; (add-batch-tests! "Problem 3.1.3" '(
;; (equals? '(1 0 0) '(1 0 0)) => #t
;; (equals? '(1 1 0) '(1 0 0)) => #f
;; (less-than? '(1 1 0) '(1 0 0)) => #f
;; (less-than? '(1 0 0) '(1 1 0)) => #t
;; (sum '(1 0 0) '(1 0 1)) => '(1 0 0 1)
;; (sum '(1 0 1) '(0)) => '(1 0 1)
;; (prod '(1 0 1) '(1 0 0)) => '(1 0 1 0)
;; (prod '(0) '(1 1 1)) => '(0)
;; ))


;; 4. Without using the interface above write native versions of the
;; functions in previous question called equals-binary?, less-than-binary?,
;; sum-binary, prod-binary.  These functions should take advantage of
;; the extra information they have about the representation to be more
;; efficient.  In particular, if n is the maximum bit length of either
;; input to these functions and c is some constant then the first two
;; functions should run in time O(n), the third O(nlog^cn), and the
;; fourth O(n2log^cn).

(define equals-binary?
  (lambda (a b)
    (equal? a b)))

(define less-than-binary?
  (lambda (a b)
    (or (< (length a) (length b))
	(and (= (length a) (length b))
	     (= (fold-left 
		 (lambda (acc head) (if (= acc 0) head acc)) 
		 0 
		 (map (lambda (x y) (- y x)) a b)) 
		1)))))

;; Helper functions for sum-binary

;; Pads a with zeroes to make it have n bits.
;; Assumes n >= a.
(define pad-zeroes
  (lambda (a n)
    (append (zeroes (- n (length a))) a)))
    
;; Left bit shift.
(define <<
  (lambda (a n)
    (append a (zeroes n))))

;; Helper to test whether all entries in a Boolean list are true.
(define all
  (lambda (f ls)
    (fold-left (lambda (acc head) (and (f head) acc)) #t ls)))

;; Helper to create zeros and ones
(define zeroes
  (lambda (n)
    (cond
     [(= n 0) '()]
     [else (cons 0 (zeroes (- n 1)))])))


;; I didn't attempt to write this to run in time O(n log^c n), but its
;; possible (think mergesort).
(define sum-binary
  (lambda (a b)
    (let 
	([len (max (length a) (length b))])
      (let
	 ([pad-a (pad-zeroes a len)]
	  [pad-b (pad-zeroes b len)])
	(let
	    ([sum-a+b-mod-2 (map (lambda (x y) (mod (+ x y) 2)) pad-a pad-b)]
	     [carry-a+b (<< (map (lambda (x y) (if (= (+ x y) 2) 1 0)) pad-a pad-b) 1)])
	  (if (all (lambda (x) (= x 0)) carry-a+b)
	      sum-a+b-mod-2
	      (sum-binary sum-a+b-mod-2 carry-a+b)))))))
  
(define prod-binary
  (lambda (a b)
    (prod-binary* a (reverse b) 0)))

(define prod-binary* 
  (lambda (a rb n)
    (cond 
     [(null? rb) (zero)]
     [(is-Zero? a) (zero)]
     [(= (car rb) 1)
      (sum-binary (<< a n) (prod-binary* a (cdr rb) (+ n 1)))]
     [else (prod-binary* a (cdr rb) (+ n 1))])))


;; (add-batch-tests! "Problem 3.1.4" '(
;; (equals?-binary '(1 0 0) '(1 0 0)) => #t
;; (equals?-binary '(1 1 0) '(1 0 0)) => #f
;; (less-than?-binary '(1 1 0) '(1 0 0)) => #f
;; (less-than?-binary '(1 0 0) '(1 1 0)) => #t
;; (sum-binary '(1 0 0) '(1 0 1)) => '(1 0 0 1)
;; (sum-binary '(1 0 1) '(0)) => '(1 0 1)
;; (prod-binary '(1 0 1) '(1 0 0)) => '(1 0 1 0)
;; (prod-binary '(0) '(1 1 1)) => '(0)
;; ))


