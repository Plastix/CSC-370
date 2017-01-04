;; Exercise 1
42

;; Exercise 2
3.1415

;; Exercise 3
"hello world"

;; Exercise 4
'("Let" "the" "Grecian" "dream" "of" "his" "sacred" "stream")

;; Exercise 5
(+ 3 4)

;; Exercise 6
(* (+ (* (/ 5 9) 4) 17) (- 6.7 13))

;; Exercise 7
(+ 0 1 2 3 4 5 6 7)
;; Exercise 8
(define x "hello world")

;; Exercise 9
(list? 6)

;; Exercise 10
(lambda (x) x)

;; Exercise 11
(define identity
  (lambda (x) x))

;; Exercise 12
(identity x)

;; Exercise 13
(let ((a 2) (b 7) (c 18))
  (/ (+ (sqrt (- (* b b) (* 4 (* a c)))) (- b)) 
     (* 2 a)))


;; Exercise 14
(define plus42 
  (lambda (e)
    (if (number? e)
      (+ e 42)
      "the answer to...")))


;; Exercise 15
(car (list 1 1 2 3 5))

;; Exercise 16
(cadddr '(1 1 2 3 5))

;; Exercise 17
(cons 1 (cons 1 (cons 2 (cons 3 5))))

;; Exercise 18
(cons 3 4)

;; Exercise 19
(cons (cons (cons 1 2) (cons 3 (cons 4 '()))) 5)

;; Exercise 20
(and (or #t #f) #t)

;; Exercise 21
(let ((a #t) (b #f) (c #f))
  (or 
    (or (not (eq? a (not b))) 
        (and c (not a))) 
    b))

;; Exercise 22
(if (string? x)
  42
  "no")

;; Exercise 23
(define positive?
  (lambda (e)
  (and (number? e) (> e 0))))

;; Exercise 24
(define numMonth->strMonth
  (lambda (n)
    (cond
      [(= n 1) "January"]
      [(= n 2) "Febuary"]
      [(= n 3) "March" ]
      [(= n 4) "April"]
      [(= n 5) "May"]
      [(= n 6) "June"]
      [(= n 7) "July"]
      [(= n 8) "August"]
      [(= n 9) "September"]
      [(= n 10) "October"]
      [(= n 11) "November"]
      [(= n 12) "December"]
      [else "Not a month!"])))

;; Exercise 25
(define member
  (lambda (e ls) 
    (cond
      [(null? ls) #f]
      [else (or (eq? (car ls) e)
                (member e (cdr ls)))])))

;; Exercise 26
(define range
  (lambda (num1 num2)
    (if (> num1 num2)
      (list)
      (cons num1 (range (+ num1 1) num2)))))

;; Exercise 27
(define append
  (lambda (ls1 ls2)
    (if (null? ls1)
      ls2
      (cons (car ls1) (append (cdr ls1) ls2)))))

;; Exercise 28
(define flatten
  (lambda (lls)
    (if (null? lls)
      (list)
      (append (car lls) (flatten (cdr lls))))))

;; Exercise 29
(define map
  (lambda (fn ls)
    (if (null? ls)
      (list)
      (cons (fn (car ls)) (map fn (cdr ls))))))

;; Exercise 30
(define filter
  (lambda (p? ls) 
     (cond
       [(null? ls) (list)]
       [(p? (car ls)) (cons (car ls) (filter p? (cdr ls)))]
       [else (filter p? (cdr ls))])))

;; Exercise 31
(define counts
  (lambda (p? ls)
    (length (filter p? ls))))

;; Exercise 32
(define fib2
  (lambda (n)
    (define (fib num)
      (cond
        [(= num 1) 0]
        [(= num 2) 1]
        [else (+ (fib (- num 1)) (fib (- num 2)))]))
    (map fib (range 1 n))))

;; Exercise 33
(define insert
  (lambda (num ls)
    (cond
      [(null? ls) (cons num '())]
      [(>= (car ls) num) (cons num ls)]
      [else (cons (car ls) (insert num (cdr ls)))])))

;; Exercise 34
(define sort
  (lambda (nums)
    (cond
      [(null? nums) (list)]
      [else (insert (car nums) (sort (cdr nums)))])))

;; Exercise 35
(define score100
  (lambda (f)
    (define looper
      (lambda (n)
        (if (>= (f n) 100)
          n
          (looper (+ n 1)))))
    (looper 0)))

;; Exercise 36
(define merge
  (lambda (nums1 nums2)
    (cond
      [(and (null? nums1) (null? nums2)) (list)]
      [(null? nums1) nums2]
      [(null? nums2) nums1]
      [(<= (car nums1) (car nums2)) (cons (car nums1) (merge (cdr nums1) nums2))]
      [else (cons (car nums2) (merge nums1 (cdr nums2)))])))

;; Exercise 37





;; Exercise 38





;; Exercise 39





