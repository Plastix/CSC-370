;; hw4-soln.ss

(load "grader.ss")

;; Define the prefix-exp datatype
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

;; The key observation:

;;     For this grammar the last minus sign always captures the two
;;     operands (either integers or larger prefix-expressions)
;;     following it.

;; For example we can parenthesize the expression from the problem
;; statement as follows:
;;      (- - 3 2 - 4 - 12 7)
;;   => (- - 3 2 - 4 [- 12 7])
;;   => (- - 3 2 (- 4 [- 12 7]))
;;   => (- (- 3 2) (- 4 [- 12 7]))

;; Our algorithm traverses the expression from the rear using a stack
;; accumulator:

;;  1.  Look at the next element of the expression:

;;      a.  If it is a integer, push a const-exp formed with that
;;          integer onto the stack.

;;      b.  If it is a minus sign, pop the first two elements off the
;;          stack, and push a diff-exp joining them onto the stack.
           
;;  2.  Repeat this until the expression is exhausted.  For a valid
;;      concrete expression, there will be exactly one remaining
;;      element on the stack.

;; Note: I coded error checking into my parser so it is slightly
;; longer than necessary.

;; (parse-prefix exp) takes an expression in the concrete syntax and
;; translates it into the abstract syntax.
(define parse-prefix
  (lambda (exp)

    ;; Call the helper function on the reverse of the expression with
    ;; an empty list plainy the role of 
    (parse* (reverse exp) '())))

(define parse*
  (lambda (exp stack)
    
    ;; Check whether we've processed the entire expression.
    (if (null? exp)      

	(if (null? (cdr stack))   ;;  equivalent to (= (length stack) 1), but faster

	    ;; There is exactly one item in the stack -- return it.
	    (car stack)           

	    ;; There is more than one item in the stack -- ERROR!
	    (eopl:error 'parse "Invalid concrete syntax.  Some stack remains: ~s~%" stack))

	;; There is more to process in the expression.
	(cond

	 ;; The next token is a minus symbol.
	 [(equal? (car exp) '-)

	  ;; Check the stack size >= 2
	  (if (and                        ;; equivalent to  ((>= (length stack) 2), but faster   
	       (not (null? stack)) 
	       (not (null? (cdr stack)))) 

	      ;; There are a sufficient number of items in the stack to form a diff-exp.
	      ;; Pop them both, push the result and recurse.
	      (parse* (cdr exp) (cons (diff-exp (car stack) (cadr stack)) (cddr stack)))

	      ;; There aren't enough items on the stack to form a diff-exp -- ERROR!
	      (eopl:error 'parse "Insufficient content on stack to minus: ~s ~s~%" exp stack))]

	 ;; The next token is an integer, push onto stack and recurse.
	 [(integer? (car exp)) (parse* (cdr exp) (cons (const-exp (car exp)) stack))]

	 ;; The next token isn't valid in the concrete syntax -- ERROR!
	 [else (eopl:error 'parse "Invalid character in concrete syntax.  Remaining stack: ~s~%" exp)]))))
	


;; Test cases
;; (add-batch-tests! "parse-prefix" '(
;; (parse-prefix '(2)) => (const-exp 2)

;; (parse-prefix '(- - 3 2 - 4 - 12 7)) 
;; => 
;; (diff-exp (diff-exp (const-exp 3) (const-exp 2)) (diff-exp (const-exp 4) (diff-exp (const-exp 12) (const-exp 7)))) 

;; (parse-prefix '(- 2 3))
;; =>
;; (diff-exp (const-exp 2) (const-exp 3))
;; ))

;; (unparse-prefix pf-exp) recurses through abstract prefix expression
;; using cases to translate back into concrete syntax. Follows the
;; grammar.
(define unparse-prefix
  (lambda (pf-exp)
    (cases prefix-exp pf-exp
      [const-exp (num) (list num)]
      [diff-exp (rand1 rand2) (cons '- (append (unparse-prefix rand1) (unparse-prefix rand2)))]
      [else (eopl:error 'parse "Invalid abstract syntax. ~s~%" pf-exp)])))

;; (add-batch-tests! "unparse-prefix" '(
;; (unparse-prefix (const-exp 2)) => '(2)
;; (unparse-prefix 
;;   (diff-exp 
;;    (diff-exp (const-exp 3) (const-exp 2)) (diff-exp (const-exp 4) (diff-exp (const-exp 12) (const-exp 7)))))
;; => 
;; '(- - 3 2 - 4 - 12 7)
;; (unparse-prefix (diff-exp (const-exp 2) (const-exp 3)))
;; =>
;; '(- 2 3)
;; ))

;; (eval-prefix pf-exp) recurses through abstract prefix expression
;; using cases to evaluate it. Follows the grammar.
(define eval-prefix
  (lambda (pf-exp)
    (cases prefix-exp pf-exp
      [const-exp (num) num]
      [diff-exp (rand1 rand2) (- (eval-prefix rand1) (eval-prefix rand2))]
      [else (eopl:error 'parse "Invalid abstract syntax. ~s~%" pf-exp)])))

;; (add-batch-tests! "eval-prefix" '(
;; (eval-prefix (const-exp 2)) => 2
;; (eval-prefix 
;;   (diff-exp 
;;    (diff-exp (const-exp 3) (const-exp 2)) (diff-exp (const-exp 4) (diff-exp (const-exp 12) (const-exp 7)))))
;; => 
;; 2
;; (eval-prefix (diff-exp (const-exp 2) (const-exp 3)))
;; =>
;; -1
;; ))




(run-all-tests!)
