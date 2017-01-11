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

;; (add-my-test! name-str qe1 qe2)
;; Function which takes a string name-str naming a test, and two
;; quoted S-expressions.  This function combines the name and the two
;; quoted expressions into a list and adds it to the head of the
;; global variable my-tests!  
;; MAKE SURE USE QUOTED EXPRESSIONS!
(define add-my-test!
  (lambda (name-str qe1 qe2)
    (set! my-tests! (cons (list name-str qe1 qe2) my-tests!))))

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

;; (run-one-test! name-str qe1 qe2)
;; Runs a test with the given name two quoted expressions
(define run-one-test!
  (lambda (name-str qe1 qe2)
    (let 
	([val1 (eval qe1)]  ;; This is why the quote are necessary.
	 [val2 (eval qe2)])
      (cond
       [(equal? val1 val2) (display-test-success! name-str qe1 qe2 val1 val2)]
       [else (display-test-failure! name-str qe1 qe2 val1 val2)]))))

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
	(let
	    ([test (car ls)])
	  (let ([name-str (car test)]
		[qe1 (cadr test)]
		[qe2 (caddr test)])
	    (run-one-test! name-str qe1 qe2)
	    (run-all-tests!* (cdr ls)))))))

  
  
;; Sample tests for functions we wrote above
;(add-my-test! "Reverse test" '(reverse '(1 2 3)) ''(3 2 1))
;(add-my-test! "Fib test" '(fib 4) '3)
;(add-my-test! "Fib test *SHOULD FAIL*" '(fib 5) ''(1 3 4)) ;; should fail

