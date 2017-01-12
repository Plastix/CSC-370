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

;; Like map but ensures that the functions are called IN ORDER
(define map*
  (lambda (fn ls)
      (reverse (fold-left (lambda (acc head) (cons (fn head) acc)) '() ls))))

;; (add-my-test! name-str ex-name-str ptval qe1 qe2)
;; Function which takes a string name-str naming a test, a string
;; ex-name-str naming the exercise, ptVal naming the point value of the problem 
;; and two quoted S-expressions. This function combines all inputs and 
;; adds it to the tail of the global variable my-tests!  
;; MAKE SURE TO USE QUOTED EXPRESSIONS!
(define add-my-test!
  (lambda (name-str ex-name-str ptval qe1 qe2)
    (set! my-tests! (append my-tests! (list (list name-str ex-name-str ptval qe1 qe2))))))

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
      (map* addTest! q-tests)))) ;; TODO don't add test if format is incorrect

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
;; Returns the test result in a 2-element list
;; (points-received, total-points)
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
    (let* ([tests (tests-with-ex ex-name-str test-ls)]
           [results (sum-results (map* run-one-test! tests))]) 
      (display-points! results))))

;; Returns all tests from test-ls matching the given ex-name-str
(define tests-with-ex
  (lambda (ex-name-str tests-ls)
    (filter (lambda (test) (equal? (test->ex-name-str test) ex-name-str)) tests-ls)))

;; Sums up points awarded and total points given a list of test results
;; See run-one-test! for test result format
(define sum-results
  (lambda (result-ls)
    (fold-left 
      (lambda (acc head) (list (+ (car acc) (car head)) (+ (cadr acc) (cadr head)))) 
      '(0 0)
      result-ls)))

;; Displays a human readable version of points award
;; Expects a two element list (points-awarded total-points)
(define display-points!
  (lambda (results)
    (display "\n")
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
(define run-all-tests!* 
  (lambda (ls)
    ; list-sort is a stable sort so sorting by exercise name will keep the
    ; with the same exercise tests in the correct order
    (let* ([sorted-tests (list-sort 
                           (lambda (t1 t2) (string<? (test->ex-name-str t1) (test->ex-name-str t2))) ls)])
      (display-points!
        (sum-results 
          (map* run-one-test! sorted-tests))))))

;(add-my-test! "Reverse test" "ex1" 10 '(reverse '(1 2 3)) ''(3 2 1)) 
;(add-my-test! "Reverse test fail" "ex1" 20 '(reverse '(1 2)) ''(3))
;(add-my-test! "Fib test" '(fib 4) '3)
;(add-my-test! "Fib test *SHOULD FAIL*" '(fib 5) ''(1 3 4)) ;; should fail
(add-batch-tests! "ex1" (list
                          (list "EX1 TEST1" '(reverse '(1 2 3)) ''(3 2 1))
                          (list "EX1 TEST2" '(reverse '(1 2)) ''(3))))

(add-my-test! "EX2 TEST1" "ex2" 1 '(length '(1 2)) '2)

(add-batch-tests! "ex2" (list
                          (list "EX2 TEST2" '(equal? 1 1) '#t)
                          (list "EX2 TEST3" '(eqv? 1 0) '#t)))

(add-my-test! "EX1 TEST3" "ex1" 1 '(length '()) '0)
