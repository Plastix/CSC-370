;; CSC 370 hw2-1 soln, Winter 2017

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

(define total-pts! 0)
(define correct-pts! 0)

(define clear-pts!
  (lambda ()
    (set! total-pts! 0)
    (set! correct-pts! 0)))

(define clear-tests! (lambda () (set! my-tests! '())))

(define clear! 
  (lambda ()
    (clear-tests!)
    (clear-pts!)))
      
;; (add-my-test! name-str qe1 qe2)
;; Function which takes a string name-str naming a test, and two
;; quoted S-expressions.  This function combines the name and the two
;; quoted expressions into a list and adds it to the head of the
;; global variable my-tests!  
;; MAKE SURE TO USE QUOTED EXPRESSIONS!
(define add-my-test! 
  (lambda (test-name-str ex-name-str ptval qe1 qe2)
    (set! my-tests! (append my-tests! (list (list test-name-str ex-name-str ptval qe1 qe2))))))


(define add-batch-tests!
  (lambda (ex-name-str q-tests)
    (cond
     [(null? q-tests)]
     [else (let
	       ([qe1 (car q-tests)]
		[qe2 (caddr q-tests)])
	     (add-my-test! "" ex-name-str 1 qe1 qe2)
	     (add-batch-tests! ex-name-str (cdddr q-tests)))])))
		

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
    (display "Success: ")
    (display-result! qe1 qe2)
    (display "\n")))

;; (display-test-failure! name-str qe1 qe2 val1 val2)
;; Displays text to indicate failure.
(define display-test-failure!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display "Failure:\n")
    (display "->Expected: ")
    (display-result! qe1 qe2)
    (display "\n-->Actuals: ")
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
       [(equal? val1 val2) (display-test-success! name-str qe1 qe2 val1 val2) #t]
       [else (display-test-failure! name-str qe1 qe2 val1 val2) #f]))))


(define run-one-exercise!
  (lambda (ex-name-str test-ls)
    (display "---------- Begin: ")
    (display ex-name-str) 
    (display " ----------\n")	     
    (let 
	([ret (my-accumulate 
	       (lambda (test acc)
		 (let 
		     ([test-name-str (car test)]
		      [test-ex-name-str (cadr test)]
		      [ptval (caddr test)]
		      [qe1 (cadddr test)]
		      [qe2 (car (cddddr test))]
		      [pts (car acc)]
		      [total (cdr acc)])
		   (cond
		    [(equal? test-ex-name-str ex-name-str)
		     (guard 
		      (except [#t 
			       (display-test-failure! test-name-str qe1 qe2 "EXCEPTION!" "EXCEPTION!")
			       (cons pts (+ ptval total))])
		      (if (run-one-test! test-name-str qe1 qe2) 
			 (cons (+ ptval pts) (+ ptval total))
			 (cons pts (+ ptval total))))]
		    [else acc])))
	       (cons 0 0)
	       test-ls)])
      (set! correct-pts! (+ correct-pts! (car ret)))
      (set! total-pts! (+ total-pts! (cdr ret)))
      (display "Correct: ")
      (display (car ret))
      (display " / ")
      (display (cdr ret))
      (display "\n"))))

   
 

(define insert-unique 
  (lambda (key ls)
    (cond
     [(null? ls) (list key)]
     [(equal? key (car ls)) ls]
     [else (cons (car ls) (insert-unique key (cdr ls)))])))

(define unique-ex-names
  (lambda (test-ls)
    (reverse
     (my-accumulate 
      (lambda (key acc) 
	(insert-unique key acc)) 
      '() 
      (map cadr test-ls)))))
  

;; (run-all-tests!)  
;; Runs all tests.  Note this is a 0-ary function, i.e., it takes no
;; arguments.
(define run-all-tests!
  (lambda ()
    (map (lambda (ex-name) (run-one-exercise! ex-name my-tests!))
	 (unique-ex-names my-tests!))
    (display "---------------------\n")
    (display "Total ")
    (display correct-pts!)
    (display " / ")
    (display total-pts!)
    (display "\n")))

;; Local definition for protection.
(define my-accumulate
  (lambda (f acc ls)
    (cond
     [(null? ls) acc]
     [else (my-accumulate f (f (car ls) acc) (cdr ls))])))

;; Actual definition
(define accumulate
  (lambda (f acc ls)
    (cond
     [(null? ls) acc]
     [else (my-accumulate f (f (car ls) acc) (cdr ls))])))



; (load "2-test.ss")
; (load "2.ss")

(display "Loaded 1-soln.ss\n")
; (display "Loaded 2-test.ss\n")
; (display "Loaded 2.ss\n")
