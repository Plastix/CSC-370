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

;; Like add-my-test! but takes a test list instead of individual arguments
;; Delegates to add-my-test!
(define add-my-test!*
  (lambda (test)
    (let ([name-str (test->name-str test)]
          [ex-name-str (test->ex-name-str test)]
          [ptval (test->ptVal test)]
          [qe1 (test->qe1 test)]
          [qe2 (test->qe2 test)])
      (add-my-test! name-str ex-name-str ptval qe1 qe2))))

(define add-batch-tests!
  (lambda (ex-name-str q-tests)
    (letrec
      ([loop (lambda (tests)
               (if (not (null? tests))
                 (let ([qes (list (car tests) (caddr tests))]) ; Pull out quoted expressions
                   (add-my-test!* (append (list "" ex-name-str 1) qes)) ; Append other test data
                   (loop (cdddr tests)))))])
      (loop q-tests))))

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
    (let ([tests (tests-with-ex ex-name-str test-ls)]) 
      (display-points! 
        (sum-results (map* run-one-test! tests))))))

;; Returns all tests from test-ls matching the given ex-name-str
(define tests-with-ex
  (lambda (ex-name-str tests-ls)
    (filter 
      (lambda (test) (equal? (test->ex-name-str test) ex-name-str)) 
      tests-ls)))

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
;; Runs all tests in the given list of tests
(define run-all-tests!* 
  (lambda (ls)
    ; exs is the ordered set of exercise strings from the list of tests
    ; The index of each string in exs is the key used to sort the tests
    ; 
    ; We first map over all tests and cons the exercise "index" to the front of the test.
    ; Then, we sort the tests by this index. We retrieve the original tests by 
    ; cdring off the index from front of the test.
    ; 
    ; list-sort is a stable sort so sorting by exercise index will keep the
    ; tests with the same exercise name in their original order
    (let* ([exs (remove-duplicates (map test->ex-name-str ls))]
           [sorted-tests (map (lambda (test) (cdr test))
                              (list-sort (lambda (t1 t2) (< (car t1) (car t2))) 
                                         (map 
                                           (lambda (test) (cons (index-of (test->ex-name-str test) exs) test)) ls)))])
      (display-points!
        (sum-results 
          (map* run-one-test! sorted-tests))))))

(define index-of
  (lambda (el ls)
    (index-of* el ls 0)))

(define index-of*
  (lambda (el ls i)
    (cond
      [(null? ls) -1]
      [(equal? el (car ls)) i]
      [else (index-of* el (cdr ls) (+ i 1))])))

(define remove-duplicates
  (lambda (ls)
    (fold-right 
      (lambda (head acc) 
        (cons head (remove head acc)))
      '()
      ls)))

;(add-my-test! "Reverse test" "ex1" 10 '(reverse '(1 2 3)) ''(3 2 1)) 
;(add-my-test! "Reverse test fail" "ex1" 20 '(reverse '(1 2)) ''(3))
;(add-my-test! "Fib test" '(fib 4) '3)
;(add-my-test! "Fib test *SHOULD FAIL*" '(fib 5) ''(1 3 4)) ;; should fail
#|(add-batch-tests! "ex1" '(|#
                          ;(reverse '(1 2 3)) => '(3 2 1)
                          ;(reverse '(1 2)) => '(3)
                          ;))

;(add-my-test! "" "ex2" 1 '(length '(1 2)) '2)

;(add-batch-tests! "ex2" '(
                          ;(equal? 1 1) => '#t
                          ;(eqv? 1 0) => '#t
                          ;))

#|(add-my-test! "" "ex1" 1 '(length '()) '0)|#
