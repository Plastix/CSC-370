;; CSC 370 HS 2
;; Aidan Pieper
;; 1/18/17

(clear-tests!)

;; GRADE
(define times10
  (lambda (nums)
    (map (lambda (x) (* x 10)) nums)))

(add-batch-tests! "EX1" '(
                          (times10 '(1 2 3 4 5)) => '(10 20 30 40 50)
                          (times10 '(25)) => '(250)
                          (times10 '()) => '()
                          (times10 '(1 1 1 1)) => '(10 10 10 10)
                          ))
;; GRADE
(define pair-up
  (lambda (elt ls)
    (map (lambda (x) (cons elt x)) ls)))

(add-batch-tests! "EX2" '(
                          (pair-up 'x '(a b c d)) => '((x . a) (x . b) (x . c) (x . d))
                          (pair-up 'a '(1 2 3)) => '((a . 1) (a . 2) (a . 3))
                          (pair-up 'a '()) => '()
                          (pair-up '(x) '(a b)) => '(((x) . a) ((x) . b)) 
                          ))
; GRADE
(define x-odds
  (lambda (nums)
    (map (lambda (n) (if (odd? n) 'x n)) 
         nums)))

(add-batch-tests! "EX3" '(
                          (x-odds '(1 2 3 4 5 9)) => '(x 2 x 4 x x)
                          (x-odds '(2 4 6)) => '(2 4 6)
                          (x-odds '(1 3 5)) => '(x x x)
                          (x-odds '()) => '()
                          ))
;; GRADE
(define replace
  (lambda (old new syms)
    (map
      (lambda (sym)
        (if (equal? old sym)
          new
          sym)) 
      syms)))

(add-batch-tests! "EX4" '(
                          (replace 'red 'blue '(red fish blue fish)) => '(blue fish blue fish)
                          (replace 'spam 'ham '(green eggs and ham)) => '(green eggs and ham)
                          (replace 'spam 'ham '(green eggs spam and ham)) => '(green eggs ham and ham)
                          (replace 'ham 'spam '(green eggs and ham)) => '(green eggs and spam)
                          (replace 'ham 'spam '(ham)) => '(spam)
                          (replace 'ham 'spam '()) => '()
                          ))
;; GRADE
(define remove
  (lambda (elt ls)
    (filter 
      (lambda (x) (not (equal? x elt)))
      ls)))

(add-batch-tests! "EX5" '(
                          (remove 'cream '(i scream for ice cream)) => '(i scream for ice)
                          (remove 'scream '(i scream you scream we all scream)) => '(i you we all)
                          (remove 'a '(a a a a a)) => '()
                          (remove 's '(1 2 3)) => '(1 2 3)
                          (remove 'a '()) => '()
                          ))
;; GRADE
(define listoflists?
  (lambda (lls)
    (fold-left
      (lambda (acc head)
        (and (list? head) acc))
      #t
      lls)))

(add-batch-tests! "EX6" '(
                          (listoflists? '((1 2 3))) => #t
                          (listoflists? '((1 2 3) 1 2 3)) => #f
                          (listoflists? '(1 2 3 (1 2 3))) => #f
                          (listoflists? '(((1) (2) (3)))) => #t
                          (listoflists? '()) => #t
                          (listoflists? '((()))) => #t
                          (listoflists? '(1 2 3 4)) => #f
                          (listoflists? '(() () ())) => #t
                          (listoflists? '((1 2 3) (1 2 3))) => #t
                          ))

;; GRADE
(define length
  (lambda (ls)
    (fold-left
      (lambda (acc head) (+ acc 1))
      0
      ls)))

(add-batch-tests! "EX7" '(
                          (length '(1 2 3)) => 3
                          (length '((1 2 3) (4 5 6))) => 2
                          (length '(1 2 3 (4 5 6))) => 4
                          (length '()) => 0
                          (length '(1)) => 1
                          (length '((()))) => 1
                          ))
(define average
  (lambda (nums)
    (let ([len (if (zero? (length nums)) 
                 1 
                 (length nums))])
      (/ (fold-right
           (lambda (acc head) (+ acc head))
           0
           nums)
         len))))

(add-batch-tests! "EX8" '(
                          (average '(1 2 3)) => 2
                          (average '(1 0 -1)) => 0
                          (average '(4 5 6 7)) => 11/2
                          (average '()) => 0
                          (average '(5)) => 5
                          (average '(-5 -5 -5 -5)) => -5
                          (average '(1/2 1/2 1/4)) => 5/12
                          ))
(define average-tr
  (lambda (nums)
    (let* ([result (fold-left
                     (lambda (acc head)
                       (list (+ (car acc) head) (+ 1 (cadr acc))))
                     '(0 0)
                     nums)]
           [sum (car result)]
           [len (if (zero? (cadr result)) 1 (cadr result))])
      (/ sum len))))

(add-batch-tests! "EX9" '(
                          (average-tr '(1 2 3)) => 2
                          (average-tr '(1 0 -1)) => 0
                          (average-tr '(4 5 6 7)) => 11/2
                          (average-tr '()) => 0
                          (average-tr '(5)) => 5
                          (average-tr '(-5 -5 -5 -5)) => -5
                          (average-tr '(1/2 1/2 1/4)) => 5/12
                          ))
(define reverse
  (lambda (ls)
    (fold-right
      (lambda (head acc)
        (append acc (list head)))
      '()
      ls)))

(add-batch-tests! "EX10" '(
                           (reverse '(1 2 3)) => '(3 2 1)
                           (reverse '((1 2) (3 4))) => '((3 4) (1 2))
                           (reverse '("hello" "world")) => '("world" "hello")
                           (reverse '()) => '()
                           ))
