;; (load "grader.ss")

;; ;; Test data
;; (define lc1 'x )
;; (define lc2 'y )
;; (define lc3 '(lambda (x) x) )
;; (define lc4 '(lambda (y) x) )
;; (define lc5 '(x y) )
;; (define lc6 '(x (y y)) )
;; (define lc7 '(x (lambda (y) y)) )
;; (define lc8 '(lambda (x) (x y)) )
;; (define lc9 '(lambda (x) (lambda (y) x)) )
;; (define lc10 '((lambda (x) x) (lambda (y) y)) )

;; Lambda Calculus Grammar
;; <exp> ::= <variable>
;;         | (lambda (<parameter>) <exp>)
;;         | (<exp> <exp>)


;; Helper functions from class
(define lambda?    ; SType -> Boolean
  (lambda (exp)
    (and (list? exp)                 
	 (equal? (length exp) 3)
	 (equal? (car exp) 'lambda)
	 (symbol? (caadr exp)))))

(define var?      ; SType -> Boolean
  (lambda (exp)
    (symbol? exp)))

(define apply?    ; SType -> Boolean
  (lambda (exp)
    (and (list? exp)
	 (equal? (length exp) 2))))

;; More helper functions
(define member?
  (lambda (x ls)
    (cond
     [(null? ls) #f]
     [(equal? x (car ls)) #t]
     [else (member? x (cdr ls))])))

     
;; 1. Write the function (get-lvars exp), which returns a list of all
;; the variables that appear in a lambda calculus expression. The
;; order that the variables appear in your answer does not matter, and
;; may include duplicates as well.
(define get-lvars
  (lambda (exp)
    (get-lvars* exp '())))

(define get-lvars*
  (lambda (exp acc)
    (cond 
     [(var? exp) (cons exp acc)]
     [(lambda? exp) (get-lvars* (caddr exp) acc)]
     [else (get-lvars* (cadr exp) (get-lvars* (car exp) acc))])))

;; (add-batch-tests! "Problem 1.1" '(
;; (get-lvars-sorted lc1)  =>  '(x)
;; (get-lvars-sorted lc4)  =>  '(x)
;; (get-lvars-sorted lc6)  =>  '(x y)
;; (get-lvars-sorted lc9)  =>  '(x)
;; (get-lvars-sorted '(lambda (x) (y z)) )  =>  '(y z)
;; (get-lvars-sorted '(z (lambda (x) x)) )  =>  '(x z)
;; (get-lvars-sorted '((lambda (f) (lambda (x) ((g (g x)) y))) z) )  =>  '(g x y z)
;; ))


;; 2. Write the function (get-lparams exp), which returns a list of
;; all the formal parameters that appear in a lambda calculus
;; expression. The order that the parameters appear in your answer
;; does not matter, and may include duplicates as well (but you'll get
;; bonus points for avoiding duplicates). However, your parameter list
;; should not contain any nested parentheses.
(define get-lparams
  (lambda (exp)
    (get-lparams* exp '())))

(define get-lparams*
  (lambda (exp acc)
    (cond 
     [(var? exp) acc]
     [(lambda? exp) (get-lparams* (caddr exp) (cons (caadr exp) acc))]
     [else (get-lparams* (cadr exp) (get-lparams* (car exp) acc))])))


;; (add-batch-tests! "Problem 1.2" '(
;; (get-lparams-sorted lc1)  =>  '()
;; (get-lparams-sorted lc4)  =>  '(y)
;; (get-lparams-sorted lc6)  =>  '()
;; (get-lparams-sorted lc9)  =>  '(x y)
;; (get-lparams-sorted '(lambda (x) (y z)) )  =>  '(x)
;; (get-lparams-sorted '((lambda (f) (lambda (x) ((g (g x)) y))) z) )  =>  '(f x)
;; (get-lparams-sorted '(lambda (x) (lambda (y) (lambda (x) z))) )   =>  '(x y)
;; ))

;; 3. Write the function (replace-vars exp), which takes a lambda
;; calculus expression exp and returns a new version of the expression
;; in which each variable has been replaced by a number indicating the
;; number of enclosing lambda expressions that surround the variable
;; in the original expression.
(define replace-vars
  (lambda (exp)
    (replace-vars* exp 0)))

(define replace-vars*
  (lambda (exp num-lambdas)
    (cond
     [(var? exp) num-lambdas]
     [(lambda? exp)
      (list 'lambda (cadr exp) 
	    (replace-vars* (caddr exp) (+ num-lambdas 1)))] 
     [else (list (replace-vars* (car exp) num-lambdas) 
		 (replace-vars* (cadr exp) num-lambdas))])))

;; (add-batch-tests! "Problem 1.3" '(
;; (replace-vars '(f x) )  =>  '(0 0)
;; (replace-vars '(lambda (x) y) )  =>  '(lambda (x) 1)
;; (replace-vars '(lambda (a) (lambda (b) c)) )  =>  '(lambda (a) (lambda (b) 2))
;; (replace-vars '(a ((lambda (a) b) c)) )  =>  '(0 ((lambda (a) 1) 0))
;; (replace-vars '((lambda (z) (lambda (y) ((lambda (x) (x y)) (x z)))) z) )
;;             => '((lambda (z) (lambda (y) ((lambda (x) (3 3)) (2 2)))) 0)
;; ))

;; 4. Write the function (free-vars exp), which takes a valid lambda
;; calculus expression as input and returns a list of all of the free
;; variables that occur in the expression. The resulting list may
;; include duplicates. A variable occurs free if it is not surrounded
;; by an enclosing lambda expression with the same formal parameter as
;; the variable. The outline of the code is given below. Notice that
;; free-vars simply passes an initial empty list to the helper
;; function free, whose job is to do the real work of recursing
;; through the expression. The formals-seen parameter is a list that
;; keeps track of all of the lambda formal parameters encountered so
;; far.  You are not allowed to use occurs-free? in your solution.
(define free-vars
  (lambda (exp)
    (free exp '())))

(define free
  (lambda (exp formals-seen)
    (cond
     [(var? exp) (if (member? exp formals-seen) '() (list exp))]
     [(lambda? exp) (free (caddr exp) (cons (caadr exp) formals-seen))]
     [else (append (free (car exp) formals-seen)
		   (free (cadr exp) formals-seen))])))


;; (add-batch-tests! "Problem 1.4"  '(
;; (free-vars-sorted 'x )  =>  '(x)
;; (free-vars-sorted '(x (y z)) )  =>  '(x y z)
;; (free-vars-sorted '((lambda (x) x) y) )  =>  '(y)
;; (free-vars-sorted '((lambda (z) z) (lambda (x) x)) )  =>  '()
;; (free-vars-sorted '((lambda (f) (lambda (x) (f x))) y) )  =>  '(y)
;; (free-vars-sorted '(lambda (x) z) )  =>  '(z)
;; (free-vars-sorted '(lambda (x) (lambda (y) ((lambda (z) (x y)) z))) )  =>  '(z)
;; ))


;; 5. Write the function (bound-vars exp), which takes a valid lambda
;; calculus expression as input and returns a list of all of the bound
;; variables that occur in the expression. The resulting list may
;; include duplicates.  A variable occurs bound if it is surrounded by
;; some enclosing lambda expression with the same formal parameter as
;; the variable. The outline of the code is given below. You are not
;; allowed to use occurs-bound? in your solution.

(define bound-vars
  (lambda (exp)
    (bound exp '())))

(define bound
  (lambda (exp formals-seen)
    (cond
     [(var? exp) (if (member? exp formals-seen) (list exp) '())]
     [(lambda? exp) (bound (caddr exp) (cons (caadr exp) formals-seen))]
     [else (append (bound (car exp) formals-seen)
		   (bound (cadr exp) formals-seen))])))

;; (add-batch-tests! "Problem 1.5" '(
;; (bound-vars-sorted 'x )  =>  '()
;; (bound-vars-sorted '(x (y z)) )  =>  '()
;; (bound-vars-sorted '((lambda (x) x) y) )  =>  '(x)
;; (bound-vars-sorted '((lambda (z) z) (lambda (x) x)) )  =>  '(x z)
;; (bound-vars-sorted '((lambda (f) (lambda (x) (f x))) y) )  =>  '(f x)
;; (bound-vars-sorted '(lambda (x) z) )  =>  '()
;; (bound-vars-sorted '(lambda (x) (lambda (y) ((lambda (z) (x y)) z))) )  =>  '(x y)
;; ))



;; 6.  Extra Credit (1pt) -- Solve exercises 1-5 above and then create
;; versions of bound-vars, free-vars, and get-lvars and called
;; bound-vars-sorted, free-vars-sorted, and get-lvars-sorted,
;; respectively,which perform the same as the original functions above
;; except that they output lists which are sorted (lexicographically)
;; and contain no duplicate variables.

(define bound-vars-sorted
  (lambda (exp)
    (clean (bound-vars exp))))

(define free-vars-sorted
  (lambda (exp)
    (clean (free-vars exp))))

(define get-lvars-sorted
  (lambda (exp)
    (clean (get-lvars exp))))

(define get-lparams-sorted
  (lambda (exp)
    (clean (get-lparams exp))))

;; A bunch of helper functions
(define clean
  (lambda (vars)
    (de-dup-vars (sort-vars vars))))

;; Sort is of variables.
(define sort-vars
  (lambda (vars)
    (mergesort (lambda (x y) (string<=? (symbol->string x) (symbol->string y))) vars)))
    
;; Helper to remove duplicates in a sorted list.
(define de-dup-vars
  (lambda (vars)
    (cond
     [(null? vars) '()]
     [(null? (cdr vars)) vars]
     [(equal? (car vars) (cadr vars))
      (de-dup-vars (cdr vars))]
     [else (cons (car vars) (de-dup-vars (cdr vars)))])))


;; Modified version of mergesort from HW 1, takes an additional input
;; which is a comparison function for the type of data in the list.
(define mergesort 
  (lambda (comp-fn ls)
    (let 
	([n (length ls)])
      (cond 
       [(<= n 1) ls]
       [else
	(let
	    ([sls (split ls (floor (/ n 2)))])
	  (merge comp-fn (mergesort comp-fn (car sls)) (mergesort comp-fn (cdr sls))))]))))

(define merge
  (lambda (comp-fn ls1 ls2)
    (cond
     [(null? ls1) ls2]
     [(null? ls2) ls1]
     [else
      (let
	  ([x1 (car ls1)]
	   [x2 (car ls2)])
	(cond
	 [(comp-fn x1 x2) (cons x1 (merge comp-fn (cdr ls1) ls2))]
	 [else (cons x2 (merge comp-fn ls1 (cdr ls2)))]))])))


(define split 
  (lambda (ls n)
    (split* ls n '())))

(define split*
  (lambda (ls n acc)
    (cond
     [(= n 0) (cons ls acc)]
     [else (split* (cdr ls) (- n 1) (cons (car ls) acc))])))
