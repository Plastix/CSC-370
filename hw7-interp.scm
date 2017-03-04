;; ref-interp.scm

(load "helpers.scm")
(load "hw7-the_grammar.scm")
(load "hw7-environment.scm")
(load "store.scm")
(load "hw7-expval.scm")

;; ==================== Evaluater ====================================

#|  Lazy evaluation define datatype by-name and by-need.
(define-datatype thunk thunk?
  (a-thunk
   (exp expression?)
   (env environment?))
)
|#

(define value-of
  (lambda (prog env)
    (cases program prog
      [a-prog (exp) (cons (value-of-exp exp env) env)]
      [def-prog (var exp) (cons (unit-val) (extend-env var (newref! (value-of-exp exp env)) env))]
      [else (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s" (car prog))])))

(define value-of-exp
  (lambda (exp env)
    (cases expression exp
      ;; In class
      [const-exp (num) (num-val num)]
      [diff-exp (exp1 exp2) (num-val (- (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]
      [zero?-exp (exp1) (bool-val (= (expval->num (value-of-exp exp1 env)) 0))]
      [if-exp (exp1 exp2 exp3) (if (expval->bool (value-of-exp exp1 env)) (value-of-exp exp2 env) (value-of-exp exp3 env))]
      [var-exp (var) (deref (apply-env env var))] ;; Implicit

      [let-exp (var exp1 exp2) (value-of-exp exp2 (extend-env var (newref! (value-of-exp exp1 env)) env))]

      ;; PROC
      [proc-exp (params body) (proc-val params body env)]
      [call-exp (rator rands)
		(let*
		    ([ratorval (value-of-exp rator env)]
		     [params (expval->proc-params ratorval)]
		     [body (expval->proc-body ratorval)]
		     [saved-env (expval->proc-saved-env ratorval)])
		  (cond
		   [(= (length params) (length rands))
		    (let*
			([pairs (map list params rands)]
			 [extend-with-pair
			  (lambda (acc head)
			    (extend-env (car head)
					(cases expression (cadr head)
					       [var-exp (var) (apply-env env var)]
					       [else (newref! (value-of-exp (cadr head) acc))])
					acc))]
			 [new-env (fold-left extend-with-pair saved-env pairs)])
		      (value-of-exp body new-env))]
		   [else (raise-exception
			  'value-of-exp 
			  "Attempt to apply function with inconsistent number of arguments: ~s ~s."
			  exp
			  exps)]
		   ))]
      
      ;; LETREC
      [letrec-exp (f-name f-params f-body body)
		  (value-of-exp body (extend-env-rec f-name f-params f-body env))]
      
      ;; HW 5
      [const-true-exp () (bool-val #t)]
      [const-false-exp () (bool-val #f)]
      [plus-exp (exp1 exp2) (num-val (+ (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]
      [div-exp (exp1 exp2)
	       (let 
		   ([val1 (expval->num (value-of-exp exp1 env))]
		    [val2 (expval->num (value-of-exp exp2 env))])
		 (if (= val2 0)
		     (raise-exception 'value-of-exp "Divide by zero exp = ~s with env = ~s" exp env)
		     (num-val (/ val1 val2))))]
      [times-exp (exp1 exp2) (num-val (* (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]
      [less-than-exp (exp1 exp2) (bool-val (< (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]
      [equal-exp (exp1 exp2) (bool-val (= (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]      
      [and-exp (exp1 exp2) (bool-val (and (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
      [or-exp (exp1 exp2) (bool-val (or (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
      [cons-exp (exp1 exp2) (list-val (cons (value-of-exp exp1 env) (expval->list (value-of-exp exp2 env)))) ]
      [not-exp (exp1) (bool-val (not (expval->bool (value-of-exp exp1 env))))]
      [car-exp (exp1)
	       (let
		   ([ls (expval->list (value-of-exp exp1 env))])
		 (if (null? ls)
		     (raise-exception 'value-of-exp "Attempting to car empty list." exp env))
		 (car ls))]
      [cdr-exp (exp1) (list-val (cdr (expval->list (value-of-exp exp1 env))))]
      [null?-exp (exp1) (bool-val (null? (expval->list (value-of-exp exp1 env))))]
      [emptylist-exp () (list-val '())]
      
      ;; Explicit References
      [newref-exp (exp) (newref! (value-of-exp exp env))]
      [deref-exp (exp) (deref (value-of-exp exp env))]
      [setref-exp (exp1 exp2) (setref! (value-of-exp exp1 env) (value-of-exp exp2 env)) (unit-val)] 
      
      ;; Implicit References
      [assign-exp (var exp) (setref! (apply-env env var) (value-of-exp exp env)) (unit-val)]
      
      [else (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))


;; ==================== Interpreter ====================================

;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n=== Welcome to the Explicit Reference Interpreter === \n\n")
			(initialize-store!)
      (read-eval-print (make-init-env)))))

;; (get-input-string) -- Reads a line from the interactive input
;; port.  Ignores zero length strings.
(define get-input-string
  (lambda ()
    (let ([str (get-line (current-input-port))])
      (if (= (string-length str) 0) 
	  (get-input-string)
	  str))))

;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Display an interpreter prompt
    (display "==> ")
    ;; Read a line user input
    (let ([code (get-input-string)])
      (cond 
       [(equal? code "!quit")
	(display "Goodbye!")  ;; Quit if 'quit entered.
	(newline)]
       [else   ;; Do something
	(cond
	 [(equal? code "!debug0")
	  (untrace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? code "!debug1")
	  (trace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? code "!debug2")
	  (trace value-of value-of-exp expval->num expval->bool expval->string)]
	 [(equal? code "!debug3")
	  (trace value-of value-of-exp expval->num expval->bool expval->string apply-env extend-env empty-env)]
	 [(equal? code "!env")
	  (display (env->string env))
	  (newline)]
	 [(equal? code "!reset-env")
	  (set! env (make-init-env))]
	 [else
	  ;; Parse code, eval expression, and print result.
	  (guard  ;; Catches parse exceptions from sllgen
	   (parse-except 
	    [else    ;; With only an else case this catches any every exception.
	     (display-exception parse-except)
	     ])
	   (let
	       ([abstract-code (parse code)])  ;; Try to parse the input line
	     (guard   ;; Catches runtime exceptions from value-of
	      (value-of-except 
	       [else
		(display-exception value-of-except)
		])
	      (let*
		  ([result (value-of abstract-code env)]
		   [val (car result)]
		   [new-env (cdr result)])
		(display (expval->string val))
		(set! env new-env)  
		(newline)
		))))])
	(read-eval-print env)]))))





