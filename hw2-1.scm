;; CSC 270 HS 2
;; Aidan Pieper
;; 1/18/17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; -- Unit Test Framework --
;;
;; This is a simple harness for unit testing Scheme code.  The basic
;; usage is to add tests in the file you are testing via the function
;; add-my-test!, and then execute the tests after loading the file using
;; (run-all-tests!).
;;
;; Note that there is a more elaborate test harness included with the
;; course software already in the file test-harness.scm.
;;
;; Feel free to use this code or a modified version to test your
;; homework assignments. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global variable for storing a list of tests, initially empty.
(define my-tests! '())

(define clear-tests! (lambda () (set! my-tests! '())))

;; (add-my-test! name-str ex-name-str ptval qe1 qe2)
;; Function which takes a string name-str naming a test, a string
;; ex-name-str naming the exercise, ptVal naming the point value of the problem 
;; and two quoted S-expressions. This function combines all inputs and 
;; adds it to the head of the global variable my-tests!  
;; MAKE SURE TO USE QUOTED EXPRESSIONS!
(define add-my-test!
  (lambda (name-str ex-name-str ptval qe1 qe2)
    (set! my-tests! (cons (list name-str ex-name-str ptval qe1 qe2) my-tests!))))

(define insert-at
  (lambda (elem pos ls)
    (cond
      [(null? ls) (list elem)]
      [(zero? pos) (cons elem ls)]
      [else (cons (car ls) (insert-at elem (- pos 1) (cdr ls)))])))

;; Tests whether the given test is in the following format:
;; (name-str qe1 qe2)
;; TODO NEEDS TO BE COMPLETED
(define valid-test?
  (lambda (test)
    (and (= (length test) 3) ; We have 3 things
         (string? (car test)))))

(define add-batch-tests!
  (lambda (ex-name-str q-tests)
    (let 
      ([addTest! (lambda (test)
                   (set! test (insert-at 1 2 (insert-at ex-name-str 1 test))) 
                   (add-my-test!  (test->name-str test) (test->ex-name-str test) (test->ptVal test) (test->qe1 test) (test->qe2 test)))])
      (map addTest! q-tests)))) ;; TODO don't add test if format is incorrect

;; Test helper methods
(define test->name-str
  (lambda (test)
    (car test)))

(define test->ex-name-str
  (lambda (test)
    (cadr test)))

(define test->ptVal
  (lambda (test)
    (caddr test)))

(define test->qe1
  (lambda (test)
    (cadddr test)))

(define test->qe2
  (lambda (test)
    (car (cddddr test))))

;; (display-result! val1 val2)
;; Takes two values and displays them.
(define display-result!
  (lambda (val1 val2)
    (display val1)
    (display " => ")
    (display val2)))

;; (display-test-success! name-str qe1 qe2 val1 val2)
;; Displays text to indicate test success.
(define display-test-success!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Success -- ")
    (display-result! qe1 qe2)
    (display "\n")))

;; (display-test-failure! name-str qe1 qe2 val1 val2)
;; Displays text to indicate failure.
(define display-test-failure!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Failure\n")
    (display "  Expected: ")
    (display-result! qe1 qe2)
    (display "\n    Actual: ")
    (display-result! qe1 val1)
    (display "\n            ")
    (display-result! qe2 val2)
    (display "\n")))

;; (run-one-test! test)
;; Runs the given test (list)
(define run-one-test!
  (lambda (test)
    (let* 
      ([name-str (test->name-str test)]
       [qe1 (test->qe1 test)]
       [qe2 (test->qe2 test)]
       [val1 (eval qe1)]  ;; This is why the quote are necessary.
       [val2 (eval qe2)]
       [ptVal (test->ptVal test)])
      (cond
        [(equal? val1 val2) (display-test-success! name-str qe1 qe2 val1 val2) (list ptVal ptVal)]
        [else (display-test-failure! name-str qe1 qe2 val1 val2) (list 0 ptVal)]))))

;; (run-one exercise! ex-name-str test-ls)
;; Runs all tests in test-ls with the exercise name equal to ex-name-str
(define run-one-exercise!
  (lambda (ex-name-str test-ls)
    (let* ([tests (filter (lambda (test)
                            (equal? (test->ex-name-str test) ex-name-str)) test-ls)]
           [results (sum-results (map run-one-test! tests))])
      (printPoints! results))))

(define sum-results
  (lambda (result-ls)
    (fold-left 
      (lambda (acc head) (list (+ (car acc) (car head)) (+ (cadr acc) (cadr head)))) 
      '(0 0)
      result-ls)))

(define printPoints!
  (lambda (results)
    (display "-----------------\n")
    (display "Total Points: ")
    (display (car results))
    (display "/")
    (display (cadr results))))

;; (run-all-tests!)  
;; Runs all tests.  Note this is a 0-ary function, i.e., it takes no
;; arguments.
(define run-all-tests!
  (lambda ()
    (run-all-tests!* my-tests!)))

;; (run-all-tests!* ls)
;; Recursive function to recurse through tests running each one
(define run-all-tests!* 
  (lambda (ls)
    (if (not (null? ls))
      (let ([test (car ls)])
        (run-one-test! test)
        (run-all-tests!* (cdr ls))))))

;; Sample tests for functions we wrote above
;(add-my-test! "Reverse test" "ex1" 10 '(reverse '(1 2 3)) ''(3 2 1)) 
;(add-my-test! "Reverse test fail" "ex1" 20 '(reverse '(1 2)) ''(3))
;(add-my-test! "Fib test" '(fib 4) '3)
;(add-my-test! "Fib test *SHOULD FAIL*" '(fib 5) ''(1 3 4)) ;; should fail
(add-batch-tests! "ex1" (list
                          (list "Reverse test" '(reverse '(1 2 3)) ''(3 2 1))
                          (list "Reverse test fail" '(reverse '(1 2)) ''(3))))
