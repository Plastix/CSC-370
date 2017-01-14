(clear-tests!)

;; Exercise 1
42

;; Exercise 2
3.14159

;; Exercise 3
"hello world"

;; Exercise 4
'("Let" "the" "Grecian" "dream" "of" "his" "sacred" "stream")

;; Exercise 5
(+ 3 4)

;; Exercise 6
(* (+ (* 4 (/ 5 9)) 17) (- 6.7 13))

;; Exercise 7
(+ 0 1 2 3 4 5 6 7)

;; Exercise 8
(define x "hello world")

(add-my-test! "Exercise 8" 'x '"hello world")


;; Exercise 9
(list? 6)

;; Exercise 10
(lambda (x) x)

;; Exercise 11
(define identity 
  (lambda (e) e))

(add-my-test! "Exercise 11.1" '(identity 1) '1)
(add-my-test! "Exercise 11.2" '(identity "hi") '"hi")

;; Exercise 12
(identity x)

;; Exercise 13
(let 
    ([a 2]
     [b 7]
     [c 18])
  (/ (+ (* -1 b) (sqrt (+ (* b b) (* 4 a c)))) (* 2 a)))

;; Exercise 14
(define plus42
  (lambda (e)
    (if (number? e)
	(+ e 42)
	"the answer to...")))

(add-my-test! "Exercise 14.1" '(plus42 1) '43)
(add-my-test! "Exercise 14.2" '(plus42 "hi") '"the answer to...")

;; Exercise 15
(car (list 1 1 2 3 5))

;; Exercise 16
(cadddr '(1 1 2 3 5))

;; Exercise 17
(cons 1 (cons 1 (cons 2 (cons 3 (cons 5 '())))))

;; Exercise 18
(cons 3 4)

;; Exercise 19
(cons (cons (cons 1 2) (cons 3 (cons 4 '())))  5)
     
;; Exercise 20
(and (or #t #f) #t)

;; Exercise 21
(let 
    ([a #t]
     [b #f]
     [c #f])
  (or (or (and (or a (not b)) (or (not a) (not(not a)))) (and c (not a))) b))

;; Exercise 22
(if (string? x) 42 "no")

;; Exercise 23
(define positive?
  (lambda (e)
    (and (number? e) (> e 0))))

(add-my-test! "Exercise 23.1" '(positive? 1) '#t)
(add-my-test! "Exercise 23.2" '(positive? 0) '#f)
(add-my-test! "Exercise 23.3" '(positive? -1) '#f)
(add-my-test! "Exercise 23.4" '(positive? "hi") '#f)


;; Exercise 24
(define numMonth->strMonth 
  (lambda (n)
    (cond
     [(eq? n 1) "January"]
     [(eq? n 2) "February"]
     [(eq? n 3) "March"]
     [(eq? n 4) "April"]
     [(eq? n 5) "May"]
     [(eq? n 6) "June"]
     [(eq? n 7) "July"]
     [(eq? n 8) "August"]
     [(eq? n 9) "September"]
     [(eq? n 10) "October"]
     [(eq? n 11) "November"]
     [(eq? n 12) "December"])))

(add-my-test! "Exercise 24.1" '(numMonth->strMonth 1) '"January")
(add-my-test! "Exercise 24.2" '(numMonth->strMonth 2) '"February")
(add-my-test! "Exercise 24.3" '(numMonth->strMonth 3) '"March")
(add-my-test! "Exercise 24.4" '(numMonth->strMonth 4) '"April")
(add-my-test! "Exercise 24.5" '(numMonth->strMonth 5) '"May")
(add-my-test! "Exercise 24.6" '(numMonth->strMonth 6) '"June")
(add-my-test! "Exercise 24.7" '(numMonth->strMonth 7) '"July")
(add-my-test! "Exercise 24.8" '(numMonth->strMonth 8) '"August")
(add-my-test! "Exercise 24.9" '(numMonth->strMonth 9) '"September")
(add-my-test! "Exercise 24.10" '(numMonth->strMonth 10) '"October")
(add-my-test! "Exercise 24.11" '(numMonth->strMonth 11) '"November")
(add-my-test! "Exercise 24.12" '(numMonth->strMonth 12) '"December")

;; Exercise 25
(define member
  (lambda (e ls)
    (cond
     [(null? ls) #f]
     [(equal? (car ls) e) #t]
     [else (member e (cdr ls))])))


(add-my-test! "Exercise 25.1" '(member 1 '(1 2 3 4)) '#t)
(add-my-test! "Exercise 25.2" '(member "hello" '(1 2 3 4)) '#f)
(add-my-test! "Exercise 25.3" '(member 3 '(1 2 (3 4))) '#f)
(add-my-test! "Exercise 25.4" '(member '(3 4) '(1 2 (3 4))) '#t)
(add-my-test! "Exercise 25.5" '(member 1 '()) '#f)

;; Exercise 26
(define range 
  (lambda (l u)
    (cond
     [(> l u) '()]
     [else (cons l (range (+ l 1) u))])))

(add-my-test! "Exercise 26.1" '(range 0 3) ''(0 1 2 3))
(add-my-test! "Exercise 26.2" '(range 3 -3) ''())
(add-my-test! "Exercise 26.3" '(range 0 0) ''(0))
(add-my-test! "Exercise 26.4" '(range 0 1) ''(0 1))

;; Exercise 27
(define append
  (lambda (ls1 ls2)
    (cond
     [(null? ls1) ls2]
     [else (cons (car ls1) (append (cdr ls1) ls2))])))


(add-my-test! "Exercise 27.1" '(append '(1 2 3 4) '(5 6 7 8)) ''(1 2 3 4 5 6 7 8))
(add-my-test! "Exercise 27.2" '(append '("alice" "bob" "claire") '(1 2 3)) 
	      ''("alice" "bob" "claire" 1 2 3))
(add-my-test! "Exercise 27.3" '(append '() '()) ''())
(add-my-test! "Exercise 27.4" '(append '() '(1)) ''(1))
(add-my-test! "Exercise 27.5" '(append '(1) '()) ''(1))


;; Exercise 28
(define flatten 
  (lambda (lls)
    (cond
     [(null? lls) '()]
     [else (flatten* (car lls) (cdr lls))])))

(define flatten*
  (lambda (ls lls)
    (cond 
     [(null? ls) (flatten lls)]
     [else (cons (car ls) (flatten* (cdr ls) lls))])))

(add-my-test! "Exercise 28.1" '(flatten '((1 2 3 4) (5 6 7 8))) ''(1 2 3 4 5 6 7 8))
(add-my-test! "Exercise 28.2" '(flatten '(((1 2) (3 4))((5) 6 7 8))) ''((1 2) (3 4) (5) 6 7 8))
(add-my-test! "Exercise 28.3" '(flatten '(())) ''())


;; Exercise 29
(define map
  (lambda (fn ls)
      (cond 
       [(null? ls) '()]
       [else (cons (fn (car ls)) (map fn (cdr ls)))])))

(add-my-test! "Exercise 29.1" '(map (lambda (x) (+ x 3)) '(1 2 3 4)) ''(4 5 6 7))
(add-my-test! "Exercise 29.2" '(map string? '("alice" "bob" 1 2 "claire" 3))  ''(#t #t #f #f #t #f))
(add-my-test! "Exercise 29.3" '(map (lambda (x) (+ x 3)) '()) ''())
(add-my-test! "Exercise 29.4" '(map null? '(() 2 () 3 () ())) ''(#t #f #t #f #t #t))


;; Exercise 30
(define filter 
  (lambda (p? ls)
    (cond
     [(null? ls) '()]
     [(p? (car ls)) (cons (car ls) (filter p? (cdr ls)))]
     [else (filter p? (cdr ls))])))

(add-my-test! "Exercise 30.1" '(filter number? '(1 2 3 4)) ''(1 2 3 4))
(add-my-test! "Exercise 30.2" '(filter atom? '(1 "hi" (2 . 3) ("hello" 4) 5)) ''(1 "hi" 5))
(add-my-test! "Exercise 30.3" '(filter atom? '()) ''())
(add-my-test! "Exercise 30.4" '(filter list? '((1 2) (3 4) 5)) ''((1 2)(3 4)))

;; Exercise 31
(define counts
  (lambda (p? ls)
    (cond
     [(null? ls) 0]
     [(p? (car ls)) (+ 1 (counts p? (cdr ls)))]
     [else (counts p? (cdr ls))])))

(add-my-test! "Exercise 31.1" '(counts atom? '(1 2 3 4)) '4)
(add-my-test! "Exercise 31.2" '(counts list? '(1 (2 3) (4 5))) '2)
(add-my-test! "Exercise 31.3" '(counts atom? '(1 "hi" (2 . 3) ("hello" 4) 5)) '3)
(add-my-test! "Exercise 31.4" '(counts atom? '()) '0)


;; Exercise 32
(define fib2
  (lambda (n)
    (cond 
     [(eq? 0 n) '()]
     [(eq? 1 n) '(0)]
     [(eq? 2 n) '(0 1)]
     [else (cons 0 (fib2* 0 0 1 1 (- n 1)))])))

(define fib2*
  (lambda (f_i-2 f_i-1 f_i i n)
    (cond 
     [(eq? i n) (list f_i)]
     [else (let ([f_i+1 (+ f_i-2 f_i-1 f_i)])
	     (cons f_i (fib2* f_i-1 f_i f_i+1 (+ i 1) n)))])))

(add-my-test! "Exercise 32.1" '(fib2 0) ''())
(add-my-test! "Exercise 32.2" '(fib2 2) ''(0 1))
(add-my-test! "Exercise 32.3" '(fib2 8) ''(0 1 1 2 4 7 13 24))
(add-my-test! "Exercise 32.3" '(fib2 10) ''(0 1 1 2 4 7 13 24 44 81))

;; Exercise 33
(define insert
  (lambda (e ls)
    (cond
     [(null? ls) (list e)]
     [(< (car ls) e) (cons (car ls) (insert e (cdr ls)))]
     [else (cons e ls)])))

(add-my-test! "Exercise 33.1" '(insert 6 '(1 2 3 4 5 7 8)) ''(1 2 3 4 5 6 7 8))
(add-my-test! "Exercise 33.2" '(insert 2 '(7 9 15)) ''(2 7 9 15))
(add-my-test! "Exercise 33.3" '(insert 20 '(7 9 15))  ''(7 9 15 20))
(add-my-test! "Exercise 33.4" '(insert 20 '())  ''(20))
(add-my-test! "Exercise 33.5" '(insert -110 '(-100 2 3 4 5 6 7 8)) ''(-110 -100 2 3 4 5 6 7 8))
(add-my-test! "Exercise 33.6" '(insert -100 '(-100 2 3 4 5 6 7 8)) ''(-100 -100 2 3 4 5 6 7 8))
(add-my-test! "Exercise 33.7" '(insert 8 '(-100 2 3 4 5 6 7 8)) ''(-100 2 3 4 5 6 7 8 8))
(add-my-test! "Exercise 33.8" '(insert 7 '(-100 2 3 4 5 6 7 7 7 8)) ''(-100 2 3 4 5 6 7 7 7 7 8))

;; Exercise 34 -- Insertion sort -- O(n^2)
(define sort
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [(insert (car ls) (sort (cdr ls)))])))

(add-my-test! "Exercise 34.1" '(sort '(7 4 6 2 1 9 4))  ''(1 2 4 4 6 7 9))
(add-my-test! "Exercise 34.2" '(sort '(7 4 6 2 -1 9 4))  ''(-1 2 4 4 6 7 9))
(add-my-test! "Exercise 34.3" '(sort '(-2 7 4 6 2 -1 9 4))  ''(-2 -1 2 4 4 6 7 9))
(add-my-test! "Exercise 34.4" '(sort '())  ''())
(add-my-test! "Exercise 34.5" '(sort '(6))  ''(6))
(add-my-test! "Exercise 34.6" '(sort '(1 2 3 4))  ''(1 2 3 4))
(add-my-test! "Exercise 34.7" '(sort '(4 4 3 3 2 2 1 1))  ''(1 1 2 2 3 3 4 4))

;; Exercise 35 
(define score100
  (lambda (f)
    (letrec 
	([score100* 
	  (lambda (n)
	    (cond 
	     [(>= (f n) 100) n]
	     [else (score100* (+ n 1))]))])
      (score100* 0))))


(add-my-test! "Exercise 35.1" '(score100 abs) '100)
(add-my-test! "Exercise 35.2" '(score100 (lambda (a) (* a a))) '10)
(add-my-test! "Exercise 35.3" '(score100 (lambda (a) (* a a a))) '5)
(add-my-test! "Exercise 35.4" '(score100 (lambda (a) 100)) '0)
(add-my-test! "Exercise 35.5" '(score100 (lambda (a) (+ (* 4 a a a) (* -1 a a) (* -100 a) -100))) '6)

;; Exercise 36 
(define merge
  (lambda (nums1 nums2)
    (cond
     [(null? nums1) nums2]
     [(null? nums2) nums1]
     [else
      (let
	  ([x1 (car nums1)]
	   [x2 (car nums2)])
	(cond
	 [(<= x1 x2) (cons x1 (merge (cdr nums1) nums2))]
	 [(> x1 x2) (cons x2 (merge nums1 (cdr nums2)))]))])))
;; Bonus: Try to do this tail recursively...

(add-my-test! "Exercise 36.1" '(merge '(1 4) '(1 2 8)) ''(1 1 2 4 8))
(add-my-test! "Exercise 36.2" '(merge '(2 6 7 8 9) '(3 4 7 9)) ''(2 3 4 6 7 7 8 9 9))
(add-my-test! "Exercise 36.3" '(merge '(2 6 7 8 9) '()) ''(2 6 7 8 9))
(add-my-test! "Exercise 36.4" '(merge '() '(2 6 7 8 9))  ''(2 6 7 8 9))       
(add-my-test! "Exercise 36.5" '(merge '() '()) ''())
(add-my-test! "Exercise 36.6" '(merge '(1) '()) ''(1))
(add-my-test! "Exercise 36.7" '(merge '() '(2)) ''(2))

;; Exercise 37
(define mergesort 
  (lambda (nums)
    (let 
	([n (length nums)])
      (cond 
       [(<= n 1) nums]
       [else
	(let
	    ([snums (split nums (floor (/ n 2)))])
	  (merge (mergesort (car snums)) (mergesort (cdr snums))))]))))

(define split 
  (lambda (nums n)
    (split* nums n '())))

(define split*
  (lambda (nums n acc)
    (cond
     [(= n 0) (cons nums acc)]
     [else (split* (cdr nums) (- n 1) (cons (car nums) acc))])))

(add-my-test! "Exercise 37.1" '(mergesort '(7 4 6 2 1 9 4))  ''(1 2 4 4 6 7 9))
(add-my-test! "Exercise 37.2" '(mergesort '(7 4 6 2 -1 9 4))  ''(-1 2 4 4 6 7 9))
(add-my-test! "Exercise 37.3" '(mergesort '(-2 7 4 6 2 -1 9 4))  ''(-2 -1 2 4 4 6 7 9))
(add-my-test! "Exercise 37.4" '(mergesort '())  ''())
(add-my-test! "Exercise 37.5" '(mergesort '(6))  ''(6))
(add-my-test! "Exercise 37.6" '(mergesort '(1 2 3 4))  ''(1 2 3 4))
(add-my-test! "Exercise 37.7" '(mergesort '(4 4 3 3 2 2 1 1))  ''(1 1 2 2 3 3 4 4))

;; Exercise 38 -- Extra Credit -- No solution provided.
;;   If anyone wants me to include their correct solution here send me
;;   an email and I'll insert it.  Obviously your name would appear in
;;   the solution.

;; Exercise 39 -- Extra Credit -- No solution provided.
;;   If anyone wants me to include their correct solution here send me
;;   an email and I'll insert it.
