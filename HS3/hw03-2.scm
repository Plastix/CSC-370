;; CSC 370 HS 370
;; Aidan Pieper
;; 1/25/17

; Load the Unit Tester
(load "unit_tester.scm") 
(clear!)

;; Exercise 1
(define zero
  (lambda () '(0)))

(define is-Zero?
  (lambda (n)
    (equal? n (zero))))

(define successor
  (lambda (n)
    (letrec ([loop
               (lambda (bits carry)
                 (cond
                   [(null? bits) (if carry (list 1) (list))]
                   [carry (if (= 1 (car bits))
                            (cons 0 (loop (cdr bits) #t))
                            (cons 1 (loop (cdr bits) #f)))]
                   [else bits]))])
      (reverse (loop (reverse n) #t)))))

(define predecessor
  (lambda (n)
    (letrec ([loop
               (lambda (bits borrow)
                 (cond
                   [(= (car bits) 1) 
                    (if (and borrow (= (length bits) 1))
                      (cdr bits)
                      (cons 0 (cdr bits)))]
                   [else (cons 1 (loop (cdr bits) #t))]))])
      (cond
        [(is-Zero? n) (zero)]
        [else (reverse (loop (reverse n) #f))]))))

(add-batch-tests! "EX1" '(
                          (zero) => '(0)
                          (is-Zero? (zero)) => #t
                          (is-Zero? '(0)) => #t
                          (is-Zero? '(1)) => #f
                          (is-Zero? "hello") => #f
                          (successor (zero)) => '(1)
                          (successor '(1)) => '(1 0)
                          (successor '(1 0)) => '(1 1)
                          (successor '(1 1)) => '(1 0 0)
                          (successor '(1 0 0)) => '(1 0 1)
                          (successor '(1 0 1)) => '(1 1 0) 
                          (successor '(1 1 0)) => '(1 1 1)
                          (successor '(1 1 1)) => '(1 0 0 0)
                          (predecessor '(0)) => '(0)
                          (predecessor '(1)) => '(0)
                          (predecessor '(1 0)) => '(1)
                          (predecessor '(1 1)) => '(1 0)
                          (predecessor '(1 0 0)) => '(1 1)
                          (predecessor '(1 0 1 1 0 0 0)) => '(1 0 1 0 1 1 1)
                          ))
;; Exercise 2
(define binary->number
  (lambda (bin)
    (letrec ([loop 
               (lambda (bits place)
                 (cond 
                   [(null? bits) 0]
                   [else (+ 
                           (if (= 1 (car bits)) 
                             (expt 2 place) 
                             0) 
                           (loop (cdr bits) 
                                 (+ place 1)))]))])
      (loop (reverse bin) 0))))

(define number->binary
  (lambda (num)
    (cond
      [(zero? num) '(0)]
      [else (number->binary* num)])))

(define number->binary*
  (lambda (num)
    (cond
      [(zero? num) '()]
      [else (append (number->binary* (floor (/ num 2))) 
                    (list (mod num 2)))])))

(add-batch-tests! "EX2" '(
                          (binary->number '(0)) => 0
                          (binary->number '(1)) => 1
                          (binary->number '(1 0)) => 2
                          (binary->number '(1 1)) => 3
                          (binary->number '(1 0 0)) => 4
                          (binary->number '(1 0 1 0 1)) => 21
                          (number->binary 0) => '(0)
                          (number->binary 1) => '(1)
                          (number->binary 2) => '(1 0)
                          (number->binary 3) => '(1 1)
                          (number->binary 4) => '(1 0 0)
                          (number->binary 21) => '(1 0 1 0 1)
                          (number->binary 64) => '(1 0 0 0 0 0 0)
                          ))
;; Exercise 3
(define equals?
  (lambda (a b)
    (cond
      [(and (is-Zero? a)
            (is-Zero? b)) #t]
      [(is-Zero? a) #f]
      [(is-Zero? b) #f]
      [else (equals? (predecessor a) 
                     (predecessor b))])))

(define less-than?
  (lambda (a b)
    (cond
      [(and (is-Zero? a)
            (is-Zero? b)) #f]
      [(is-Zero? a) #t]
      [(is-Zero? b) #f]
      [else
        (less-than? (predecessor a) 
                    (predecessor b))])))

(define sum
  (lambda (a b)
    (cond
      [(is-Zero? b) a]
      [else (sum (successor a) (predecessor b))])))

(define prod
  (lambda (a b)
    (cond
      [(or (is-Zero? a)
           (is-Zero? b)) 
       (zero)]
      [else (sum a (prod a (predecessor b)))])))

(add-batch-tests! "EX3" '(
                          (equals? (zero) (zero)) => #t
                          (equals? (zero) '(1)) => #f
                          (equals? '(1) (zero)) => #f
                          (equals? '(1 0) '(1)) => #f
                          (equals? '(1) '(1 0)) => #f
                          (equals? '(1 0 1 0 1) '(1 0 1 1 1)) => #f
                          (equals? '(1 0 1 1 1) '(1 0 1 0 1)) => #f
                          (equals? '(1 1 0 0 1) '(1 1 0 0 1)) => #t
                          (equals? '(1 1 1 0) '(1 0 0 1 0 0)) => #f
                          (equals? '(1 0 0 1 0 0) '(1 1 1 0)) => #f
                          (equals? '(1 1 1 1 1) '(1 1 1 1 1)) => #t
                          (less-than? (zero) (zero)) => #f
                          (less-than? (zero) '(1)) => #t
                          (less-than? '(1) (zero)) => #f
                          (less-than? '(1) '(1)) => #f
                          (less-than? '(1 0) '(1 1 1)) => #t
                          (less-than? '(1 1 1) '(1 0)) => #f
                          (less-than? '(1 0 0 0) '(1 1 1)) => #f
                          (less-than? '(1 1 1) '(1 0 0 0)) => #t
                          (less-than? '(1 0 1 1 1) '(1 0 1 1 1)) => #f
                          (less-than? '(1 1 1 1) '(1 0 0 0 0)) => #t
                          (less-than? '(1 0 0 0 0) '(1 1 1 1)) => #f
                          (less-than? '(1 0 1 1 1 1) '(1 0 0 1 1 1)) => #f
                          (less-than? '(1 0 0 1 1 1) '(1 0 1 1 1 1)) => #t
                          (sum (zero) (zero)) => (zero)
                          (sum (zero) '(1)) => '(1)
                          (sum '(1) (zero)) => '(1)
                          (sum '(1) '(1)) => '(1 0)
                          (sum '(1 1) '(1 1)) => '(1 1 0)
                          (sum '(1 0 0) '(1)) => '(1 0 1)
                          (sum '(1) '(1 0 0)) => '(1 0 1)
                          (sum '(1 0 1 0) '(1 1 1)) => '(1 0 0 0 1)
                          (sum '(1 1 1) '(1 0 1 0)) => '(1 0 0 0 1)
                          (prod (zero) (zero)) => (zero)
                          (prod (zero) '(1 0)) => (zero)
                          (prod '(1 0) (zero)) => (zero)
                          (prod '(1) '(1 0)) => '(1 0)
                          (prod '(1 0) '(1)) => '(1 0)
                          (prod '(1 1 0) '(1 0 0 0)) => '(1 1 0 0 0 0)
                          (prod '(1 0 0 0) '(1 1 0)) => '(1 1 0 0 0 0)
                          (prod '(1) '(1)) => '(1)
                          (prod '(1 0 0 0 0 0 0) '(1 0 0 0 0 0 0)) => '(1 0 0 0 0 0 0 0 0 0 0 0 0))
                  
                  ) 
;; Exercise 4
(define equals-binary?
  (lambda (a b)
    (cond
      [(not (= (length a)
               (length b))) #f]
      [(and (null? a)
            (null? b)) #t]
      [(= (car a) (car b)) 
       (equals-binary? (cdr a) (cdr b))]
      [else #f])))

(define less-than-binary?
  (lambda (a b)
    (let ([alen (length a)]
          [blen (length b)])
      (cond
        [(< alen blen) #t]
        [(> alen blen) #f]
        [(or (null? a)
             (null? b)) #f]
        [(= (car a) (car b)) 
         (less-than-binary? (cdr a) (cdr b))]
        [else (< (car a) (car b))]))))

(define sum-binary
  (lambda (a b)
   (reverse (sum-binary* (reverse a) (reverse b) #f ))))

(define sum-binary*
  (lambda (a b carry)
    (cond
      [(null? b) (if carry (reverse (successor a)) a)]
      [(null? a) (if carry (reverse (successor b)) b)]
      [else 
        (let* ([a1 (= (car a) 1)]
               [b1 (= (car b) 1)]
               [bits (list a1 b1 carry)])
          (cond
            [(exactly-n-true? 1 bits) (cons 1 (sum-binary* (cdr a) (cdr b) #f))]
            [(exactly-n-true? 2 bits) (cons 0 (sum-binary* (cdr a) (cdr b) #t))]
            [(and a1 b1 carry) (cons 1 (sum-binary* (cdr a) (cdr b) #t))]
            [else (cons 0 (sum-binary* (cdr a) (cdr b) #f))]))])))

(define exactly-n-true?
  (lambda (n ls)
    (= (length (remove #f ls)) n)))


(define prod-binary
  (lambda (a b)
      (cond
       [(or (is-Zero? a)
            (is-Zero? b)) (zero)]
       [else (prod-binary* a (reverse b) 0)])))

(define prod-binary*
  (lambda (a b n)
    (cond
      [(null? b) (zero)]
      [else (sum-binary
              (if (= (car b) 1)
                (if (is-Zero? a)
                  (zero)
                  (append a (repeat 0 n)))
                (zero))
              (prod-binary* a (cdr b) (+ n 1)))])))

(define repeat
  (lambda (sym n)
    (cond
      [(zero? n) (list)]
      [else (cons sym (repeat sym (- n 1)))])))

(add-batch-tests! "EX4" '(
                          (equals-binary? (zero) (zero)) => #t
                          (equals-binary? (zero) '(1)) => #f
                          (equals-binary? '(1) (zero)) => #f
                          (equals-binary? '(1 0) '(1)) => #f
                          (equals-binary? '(1) '(1 0)) => #f
                          (equals-binary? '(1 0 1 0 1) '(1 0 1 1 1)) => #f
                          (equals-binary? '(1 0 1 1 1) '(1 0 1 0 1)) => #f
                          (equals-binary? '(1 1 0 0 1) '(1 1 0 0 1)) => #t
                          (equals-binary? '(1 1 1 0) '(1 0 0 1 0 0)) => #f
                          (equals-binary? '(1 0 0 1 0 0) '(1 1 1 0)) => #f
                          (equals-binary? '(1 1 1 1 1) '(1 1 1 1 1)) => #t
                          (less-than-binary? (zero) (zero)) => #f
                          (less-than-binary? (zero) '(1)) => #t
                          (less-than-binary? '(1) (zero)) => #f
                          (less-than-binary? '(1) '(1)) => #f
                          (less-than-binary? '(1 0) '(1 1 1)) => #t
                          (less-than-binary? '(1 1 1) '(1 0)) => #f
                          (less-than-binary? '(1 0 0 0) '(1 1 1)) => #f
                          (less-than-binary? '(1 1 1) '(1 0 0 0)) => #t
                          (less-than-binary? '(1 0 1 1 1) '(1 0 1 1 1)) => #f
                          (less-than-binary? '(1 1 1 1) '(1 0 0 0 0)) => #t
                          (less-than-binary? '(1 0 0 0 0) '(1 1 1 1)) => #f
                          (less-than-binary? '(1 0 1 1 1 1) '(1 0 0 1 1 1)) => #f
                          (less-than-binary? '(1 0 0 1 1 1) '(1 0 1 1 1 1)) => #t                        
                          (sum-binary (zero) (zero)) => (zero)
                          (sum-binary (zero) '(1)) => '(1)
                          (sum-binary '(1) (zero)) => '(1)
                          (sum-binary '(1) '(1)) => '(1 0)
                          (sum-binary '(1 1) '(1 1)) => '(1 1 0)
                          (sum-binary '(1 0 0) '(1)) => '(1 0 1)
                          (sum-binary '(1) '(1 0 0)) => '(1 0 1)
                          (sum-binary '(1 0 1 0) '(1 1 1)) => '(1 0 0 0 1)
                          (sum-binary '(1 1 1) '(1 0 1 0)) => '(1 0 0 0 1)
                          (prod-binary (zero) (zero)) => (zero)
                          (prod-binary (zero) '(1 0)) => (zero)
                          (prod-binary '(1 0) (zero)) => (zero)
                          (prod-binary '(1) '(1 0)) => '(1 0)
                          (prod-binary '(1 0) '(1)) => '(1 0)
                          (prod-binary '(1 1 0) '(1 0 0 0)) => '(1 1 0 0 0 0)
                          (prod-binary '(1 0 0 0) '(1 1 0)) => '(1 1 0 0 0 0)
                          (prod-binary '(1) '(1)) => '(1)
                          (prod-binary '(1 0 0 0 0 0 0) '(1 0 0 0 0 0 0)) => '(1 0 0 0 0 0 0 0 0 0 0 0 0)
                          )) 
