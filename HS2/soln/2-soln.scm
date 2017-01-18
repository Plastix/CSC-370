;; CSC 370 hw2-2 soln, Winter 2017

;; 1. Using map, write the function (times10 nums), which takes a list of
;; numbers and returns a new list with each number multiplied by 10,
;; e.g., see below (in the batch format expected by the extended unit
;; tester of Problem 1).

(define times10 
  (lambda (nums) "not implemented"))

(add-batch-tests! "Exercise 1 (6pts)" '(
(times10 '(1 2 3 4 5))  =>  '(10 20 30 40 50)
(times10 '(-1 2 -1 4 4+i))  =>  '(-10 20 -10 40 40+10i)
(times10 '(25))  =>  '(250)
(times10 '(25 3.5))  =>  '(250 35.0)
(times10 '()) => '()
(times10 '()) => '()
))

;; 2. Using map, write the function (pair-up elt ls), which pairs elt
;; with each item in ls and returns a list of the pairs, e.g.:

(define pair-up 
  (lambda (elt ls)
    (map (lambda (head) (cons elt head)) ls)))

(add-batch-tests! "Exercise 2 (6pts)" '(
(pair-up 'x '(a b c d))  =>  '((x . a) (x . b) (x . c) (x . d))
(pair-up 'x '(a b c d))  =>  '((x . a) (x . b) (x . c) (x . d))
(pair-up 'a '(1 2 3))  =>  '((a . 1) (a . 2) (a . 3))
(pair-up '() '(1 2 3)) => '((() . 1) (() . 2) (() . 3))
(pair-up 'a '())  =>  '() 
(pair-up 'a '())  =>  '() 
))

;; 3. Using map, write the function (x-odds nums), which replaces each
;; odd element of nums with the symbol x, e.g.:

(define x-odds
  (lambda (nums)
    (map 
     (lambda (head) 
       (if (and 
	    (integer? head) 
	    (= (mod head 2) 1))
	   'x
	   head))
     nums)))

(add-batch-tests! "Exercise 3 (6pts)" '(
(x-odds '(1 2 3 4 5 9))  =>  '(x 2 x 4 x x)
(x-odds '(1 2 3 4 5 9))  =>  '(x 2 x 4 x x)
(x-odds '(2 4 6))  =>  '(2 4 6)
(x-odds '(1 3 5))  =>  '(x x x)
(x-odds '())  =>  '()
(x-odds '())  =>  '()
))

;; 4.  Using map, write the function (replace old new syms), which
;; replaces all occurrences of the symbol old with the symbol new
;; within a list syms of symbols.

(define replace
  (lambda (old new syms)
    (map 
     (lambda (head)
       (if (equal? head old) 
	   new
	   head))
     syms)))

(add-batch-tests! "Exercise 4 (6pts)" '(
(replace 'red 'blue '(red fish blue fish))  =>  '(blue fish blue fish)
(replace 'red 'blue '(fish fish fish fish))  =>  '(fish fish fish fish)
(replace 'spam 'ham '(green eggs and ham))  =>  '(green eggs and ham)
(replace 'ham 'spam '(green eggs and ham))  =>  '(green eggs and spam) 
(replace 'ham 'spam '())  =>  '() 
(replace 'ham 'spam '())  =>  '() 
))

;; 5. Using filter, write the function (remove elt ls), which removes
;; all occurrences of elt from the list ls.

(define remove
  (lambda (elt ls)
    (filter (lambda (head) (not (equal? elt head))) ls)))
	   
	      
(add-batch-tests! "Exercise 5 (6pts)" '(
(remove 'cream '(i scream for ice cream))  =>  '(i scream for ice)
(remove 'cream '())  =>  '()
(remove 'cream '())  =>  '()
(remove "cream" '("i" "scream" "for" "ice" "cream"))  =>  '("i" "scream" "for" "ice")
(remove 'x '(1 x 2 x 3 x 4 x 5))  =>  '(1 2 3 4 5)
(remove 'scream '(i scream you scream we all scream))  =>  '(i you we all)
))

; 6. Using the function fold-left, write the predicate (listoflists?
; lls), which returns true if, and only if, lls is a list of lists,
; e.g.:

(define listoflists?
  (lambda (lls)
    (fold-left (lambda (acc head) (and (list? head) acc)) #t lls)))

(add-batch-tests! "Exercise 6 (6pts)" '(
(listoflists? '((1 2 3))) => #t
(listoflists? '((1 2 3) 1 2 3)) => #f
(listoflists? '(((1))(2)((3)))) => #t
(listoflists? '()) => #t
(listoflists? '()) => #t
(listoflists? '(1)) => #f
))

;; 7. Using the function fold-left, write the function (length ls),
;; which returns the length of the list ls, e.g.:

(define length
  (lambda (ls)
    (fold-left (lambda (acc head) (+ 1 acc)) 0 ls)))

(add-batch-tests! "Exercise 7 (6pts)" '(
(length '(1 2 3)) => 3
(length '(1 2 3)) => 3
(length '((1 2 3)(4 5 6))) => 2
(length '(1 2 3 (4 5 6))) => 4
(length '()) => 0
(length '()) => 0
))

;; 8. Using the function fold-right, write the function (average
;; nums), which computes the average of the numbers in nums, e.g.:

(define average
  (lambda (nums)
    (if (= (length nums) 0) 
	0
	(/ (fold-right
	    +
	    0 
	    nums) 
	   (length nums)))))

(add-batch-tests! "Exercise 8 (6pts)" '(
(average '(1 2 3)) => 2
(average '(1 0 -1)) => 0
(average '(4 5 6 7)) => 11/2 
(average '(4 5 6 7)) => 11/2 
(average '()) => 0
(average '()) => 0
))

;; 9. Using the function fold-left, write a tail-recursive function
;; (average-tr nums), which computes the average of the numbers in
;; nums -- you may not compute the length of nums before calling
;; accumulate, e.g.:

(define average-tr
  (lambda (nums)
    (fold-left
     (lambda (acc head) 
       (if (symbol? head)
	   (if (equal? (cdr acc) 0)
	       0
	       (/ (car acc) (cdr acc)))
	   (cons (+ head (car acc)) (+ (cdr acc) 1)))) 
     (cons 0 0) 
     (reverse (cons 'end nums)))))


(add-batch-tests! "Exercise 9 (6pts)" '(
(average '(1 2 3)) => 2
(average '(1 0 -1)) => 0
(average '(4 5 6 7)) => 11/2 
(average '(4 5 6 7)) => 11/2 
(average '()) => 0
(average '()) => 0
))

;; 10. Using the function fold-right, write a function (reverse ls),
;; which returns a list containing the elements of ls in reverse
;; order, e.g.,:

(define reverse
  (lambda (ls)
    (fold-right (lambda (head acc) (append acc (list head))) '() ls)))

(add-batch-tests! "Exercise 10 (6pts)" '(
(reverse '(1 2 3)) => '(3 2 1)
(reverse '()) => '()
(reverse '()) => '()
(reverse '((1 2)(3 4))) => '((4 4)(1 2))
(reverse '((1 2)(3 4))) => '((4 4)(1 2))
(reverse '("hello" "world")) => '("world" "hello")
))
