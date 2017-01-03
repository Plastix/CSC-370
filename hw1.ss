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
(+ (+ (+ (+ (+ (+ (+ 0 1) 2) 3) 4) 5) 6) 7)

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
     (* 2 a))
  )


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
    (if (and (number? e) (> e 0))
      #t
      #f
      )))

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
;; TODO Replace with ifs with cond
(define member
  (lambda (e ls) 
    (if (null? ls)
       #f
       (if (eq? (car ls) e)
         #t
         (member e (cdr ls))))))

;; Exercise 26
(define range
  (lambda (num1 num2)
    (if (> num1 num2)
      (list)
      (cons num1 (range (+ num1 1) num2)))))
