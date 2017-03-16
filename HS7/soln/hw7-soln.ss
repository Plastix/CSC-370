;; hw7-soln.ss

(load "hw7-includes.ss")

;; ================== General Helper Functions ========================

(define accumulate
  (lambda (f acc ls)
    (cond
     [(null? ls) acc]
     [else (accumulate f (f (car ls) acc) (cdr ls))])))

;; =============== Environment Definition =============================


;; This is an implementation of the var-val pair list representation
;; of an environment, we wrote earlier.  I translated the
;; representation into a define-datatype so we get the constructors
;; and type checking predicate for free, and can use cases to process.

(define-datatype environment environment?
  (empty-env)                   ;; (empty-env) gives an empty environment
  (extend-env                   ;; (extend-env var val env) extends the environment
   (var symbol?)
   (val expval?)
   (env environment?))
  (extend-env-rec                                                                                    
    (p-name symbol?)                                                                                 
    (p-vars (list-of symbol?))                                                                                  
    (p-body expression?)                                                                             
    (env environment?)) 
)

;; (apply-env env target-var) s to figure out the maping of target-var                               
;; in the environment env.                                                                           
(define apply-env ; Env x Var -> SType                                                               
  (lambda (env target-var)                                                                           
    (cases environment env                                                                           
      [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]                     
      [extend-env (var val env*)                                                                     
         (cond                                                                                       
           [(equal? var target-var) val]                                                             
           [else (apply-env env* target-var)])]                                                      
      [extend-env-rec (p-name p-vars p-body env*)                                                     
         (cond                                                                                       
           [(equal? p-name target-var)                                                               
	    (newref! (proc-val (procedure p-vars p-body env)))]                                                
           [else (apply-env env* target-var)])])))      

;; ============== Environment Helper Functions ======================

(define make-init-env
  (lambda ()
    (extend-env 
     'pi (newref! (num-val 3.14159))
     (extend-env
      'e (newref! (num-val 2.71828))
      (empty-env)))))

(define env->string
  (lambda (env)
    (cases environment env 
	   [empty-env () "[]"] 
	   [extend-env (var val env*)
		       (string-append "[" (symbol->string var)
				      " = " (expval->string val)
				      (env->string* env*) "]")] 
	   [extend-env-rec (p-name p-var p-body env*)
			   (string-append "[" (symbol->string p-name) " = #recproc" (env->string* env*) "]")] )))

(define env->string*
  (lambda (env)
    (cases environment env 
	   [empty-env () ""] 
	   [extend-env (var val env*)
		       (string-append ", " (symbol->string var)
				      " = " (expval->string val)
				      (env->string* env*))] 
	   [extend-env-rec (p-name p-var p-body env*)
			   (string-append ", " (symbol->string p-name) " = #recproc" (env->string* env*))] )))


;; =============== Store Cells ========================================

;; In order to keep track of all the bookkeeping in our store we
;; introduce a new datatype that abstracts store cells that contain
;; values and store cells that are empty.  The marker on val-cells is
;; either true or false.  Cells that are marked don't match the value
;; of the global variable the-marker!, defined below.

(define-datatype store-cell storeval?
  (val-cell
   (ev expval?)
   (marker boolean?))
  (free-cell
   (next integer?))
)

;; (store-cell->next cell)
;; An accessor function for free-cells.  If cell is free-cell, it
;; return the next field, other it behaves arbitrarily.
(define store-cell->next
  (lambda (cell)
    (cases store-cell cell
     [free-cell (next) next]
     [else -1]
)))

;; (store-cell->string cell)
;; A function that transforms a store-cell cell to a string.
;; Unmarked cell are indicated by *'s.
(define store-cell->string
  (lambda (cell)
    (cases store-cell cell
     [free-cell (next) (string-append "#free(" (number->string next) ")")]
     [val-cell (ev marker) 
      (cond
       [(equal? marker the-marker!) (expval->string ev)]
       [else (string-append "*" (expval->string ev) "*")])])))
	  

;; ================ Store ============================================

;; the-store! is the store!
(define the-store! 'uninitialized)
;; Stores an integer index into the store indicating the first free
;; cell.  This value is -1 if there are no free cells.
(define next-free! 'uninitialized)
;; Stores the current value of the marker being use to indicate
;; whether a cell is free, alternates between true and false, so that
;; we don't have to reset the markers on each cell before we collect
;; the garbage.
(define the-marker! 'uninitialized)

;; ----- Several Functions for Displaying the Store -----------------

(define display-the-store!
  (lambda ()
    (display (the-store!->string))
    (newline)
    ))

(define the-store!->string
  (lambda ()
    (string-append "|" (the-store!->string* 0) "|")))

(define the-store!->string*
  (lambda (index)
    (cond
     [(= index (- (vector-length the-store!) 1))
      (store-cell->string (vector-ref the-store! index))]
     [else 
      (string-append 
       (store-cell->string (vector-ref the-store! index))
       ", "
       (the-store!->string* (+ index 1)))])))

;; ---- Updated Store Interface -------------------------------------

;; Updated the store to use Scheme vectors instead of Scheme lists.
;; Since vectors are pass by reference we have to manually resize them
;; when a new reference is requested but the store is full.  This is
;; primarily accomplished by the double-store! function below.  The
;; newref! and initialization functions have to do extra work to
;; maintain the skip list of free cells -- essentially using the first
;; free cell in the skip list and then updating the next free cell to
;; be the one the first free cell referenced.
	      
;; (empty-store) return a Scheme vector containing an empty store with
;; a single free cell.
(define empty-store
  (lambda ()
    (vector (free-cell -1))))

;; (initialize-store!) -- For effect only!
;; Initializes the store. Sets the-store! to (empty-store), next-free!
;; to 0, and the-marker! to #t.
(define initialize-store!
  (lambda ()
    (set! the-store! (empty-store))
    (set! next-free! 0)
    (set! the-marker! #t)))

;; (double-store!) -- For effect only!
;; Doubles the size of the store by copying the current contents into
;; a new vector.  It also update the skip list of free cells with the
;; newly allocated space (this uses sweep! to save on code).
(define double-store!
  (lambda ()
    (set! the-store! 
	  (list->vector (append (vector->list the-store!) 
				(vector->list (make-vector 
					       (vector-length the-store!) 
					       (val-cell (unit-val) (not the-marker!)) 
					       ;; This is a hack so I can use sweep! to set up the
					       ;; skip list of free cells.
					       )))))
    (sweep!) ;; Sets up the free cells.
))

;; (newref! val) 
;; Takes a value val and adds it to the the-store!.  It returns a
;; ref-val reference the added value.
(define newref!
  (lambda (val)
    (if (= next-free! -1) (double-store!))  ;; Double the store if necessary.
    (let
	([next (store-cell->next (vector-ref the-store! next-free!))]
	 [index next-free!])
      (vector-set! the-store! next-free! (val-cell val the-marker!)) ;; Insert the value.
      (set! next-free! next)  ;; Update the free list
      (ref-val index))))

;; (deref ev) expects that ev is a reference (ref-val ref), and
;; returns the value of associated with ref in the store.
(define deref
  (lambda (ev)
    (cases store-cell (vector-ref the-store! (expval->ref ev))
      [val-cell (ev marker) ev]
      [free-cell (next) 
       (raise-exception 'deref "Segmentation Fault: attempt to dereference unallocated memory ~s" ev)])))

;; (setref! ev val) expects that ev is a reference (ref-val ref), and
;; it sets the reference ref to val in the the-store!
(define setref!
  (lambda (ev val)
    (vector-set! the-store! (expval->ref ev) (val-cell val the-marker!))))


;; ==================== Garbage Collector =================================

;; This is a simple implementation of a static tracing garbage
;; collector.  It takes an environment then marks all cells in the
;; store that can be reached by recursively traversing references and
;; saved environments that it encounters.  The first time the process
;; reaches a given cell is it marked and then recursively marks any
;; cells reachable from it.  The recursion stops at marked cells to
;; prevent infinite loops.  Once all reachable cells have been
;; determined the garbage collector frees all unmarked cells.  The
;; garbage collector does NOT attempt to shrink the store once
;; sufficiently few cells are in use.

;; Below are some interesting test cases with the state of the store
;; indicated after each command.  Test 6 demonstrates how wasteful
;; static garbage collection can be.

#|
---------- Test 1 ----------------

==> 1 
1
==> !store
|3.14159, 2.71828|

---------- Test 2 ----------------

==> let x = 1 in x
1
==> !store
|3.14159, 2.71828, #free(3), #free(-1)|

---------- Test 3 ----------------

==> let x = 1 in proc(y) x                                
#proc(1)
==> !store
|3.14159, 2.71828, #free(3), #free(-1)|

---------- Test 4 ----------------

==> def! f = let x = 15 in let g = proc(y) x in g
#unit
==> !store
|3.14159, 2.71828, 15, #free(5), #proc(1), #free(6), #free(7), #free(-1)|
==> (f 10)
15
==> !store
|3.14159, 2.71828, 15, #free(5), #proc(1), #free(6), #free(7), #free(-1)|

---------- Test 5 ----------------

==> def! r = let r1 = newref!(0) in let r2 = newref!(r1) in {setref!(r1,r2) r1}
#unit
==> !store
|3.14159, 2.71828, #ref(4), #free(5), #ref(2), #free(7), #ref(2), #free(-1)|
==> deref(r)
#ref(4)
==> deref(deref(r))
#ref(2)
==> deref(deref(deref(r)))
#ref(4)
==> !store
|3.14159, 2.71828, #ref(4), #free(5), #ref(2), #free(7), #ref(2), #free(-1)|

---------- Test 6 ----------------

==> def! fact = letrec f(x) = if zero?(x) then 1 else *(x,(f -(x,1))) in f
#unit
==> !store
|3.14159, 2.71828, #free(-1), #proc(1)|
==> (fact 10)
3628800
==> !store
|3.14159, 2.71828, #free(4), #proc(1), #free(5), #free(6), #free(7), #free(8), #free(9), #free(10), #free(11), #free(12), #free(13), #free(14), #free(15), #free(16), #free(17), #free(18), #free(19), #free(20), #free(21), #free(22), #free(23), #free(24), #free(25), #free(26), #free(27), #free(28), #free(29), #free(30), #free(31), #free(-1)|

|#


;; ----- Helper Functions for Vectors ------------------------------

;; (map-vector! f vec) -- For effect only!
;; Applies the 2-input function f to each element of the vector vec
;; from the end of vec to the beginning.  The first argument passed to
;; f is the value of the vector cell currently being examined and the
;; second is the index of that cell.
(define map-vector!
  (lambda (f vec)
    (map-vector!* f vec (- (vector-length vec) 1))))

;; (map-vector! f vec index) Helper function for map-vector! which
;; additionally takes the current index being considered.
(define map-vector!*
  (lambda (f vec index)
    (cond
     [(>= index 0)
      (vector-set! vec index (f (vector-ref vec index) index))
      (map-vector!* f vec (- index 1))])))

;; ----- Dual Recursive Marking Functions --------------------------


;; (mark-env! env) -- For effect only!
;; Marks everything in the store reachable from the environment env.
;; Dual recursive with mark-expval!
(define mark-env! 
  (lambda (env)
    (cases environment env
     ;; For the empty environment we do nothing because nothing in the
     ;; store can be reached from it.
     [empty-env () 0] 
     ;; For an extend-env we first mark all the elements of the store
     ;; reachable from the value and then mark everything reachable
     ;; from the rest of the environment.
     [extend-env (var val env*)
		 (mark-expval! val)
		 (mark-env! env*)]
     ;; For an extend-env-rec we just mark the rest of the environment
     ;; because none of the field can contain any references that
     ;; wouldn't already be in the rest of the environment.
     [extend-env-rec (p-name p-vars p-body env*) (mark-env! env*)]
)))
     
;; (mark-expval! ev) -- For effect only!
;; Marks everything in the store reachable from the expval ev.
;; Dual recursive with mark-env!
(define mark-expval! 
  (lambda (ev)
    (cases expval ev
     ;; For a procedure we mark the cells reachable via the saved
     ;; environment.
     [proc-val (p) 
       (cases proc p
	[procedure (vars body env) (mark-env! env)])] 
     ;; For a reference we mark the cell if is isn't already marked.
     ;; If it wasn't (i.e., this is the first visit to the cell) we
     ;; mark everything reachable from it.
     [ref-val (ref) 
      (cases store-cell (vector-ref the-store! ref)
       [val-cell (ev marker) 
	(cond
	 [(equal? marker the-marker!) ;; This cell hasn't be visited already.
	  (vector-set! the-store! ref (val-cell ev (not the-marker!))) 
	  (mark-expval! ev)]
	 [else 0])]
       [else (raise-exception 'mark-expval! "Doh! A free cell is reachable!")])]
     
     ;; For any other expval we do nothing because it can't reference
     ;; anything else in the store.
     [else 0] 
)))

;; ----- Sweeping --------------------------------------------------

;; (sweep!) -- For effect only!
;; Process the-store! eliminate any val-cells whose marker matches
;; the-marker!.  Deletes these cells by replacing them by free-cells
;; that are added to the skip list.
(define sweep!
  (lambda ()
    (map-vector!
     (lambda (cell index)
       (cases store-cell cell
	 [val-cell (ev marker) 
	   (cond
	    [(equal? marker the-marker!) cell]
	    [else (let 
		      ([new-cell (free-cell next-free!)]) 
		    (set! next-free! index) ;; Update skip list.
		    new-cell)])]  
	 [else cell]))
     the-store!)))


;; ----- Main Garbage Collector --------------------------------------

;; (garbage-collect! env) -- For effect only!
;; Implements tracing garbage collection by recursing over the
;; environment marking the elements of the store reachable from the
;; given environment.  It then deletes all cells of the store not
;; reachable from the given environment.  These cells are added to the
;; free list.
(define garbage-collect!
  (lambda (env)
    ;; Mark the reachable cells in the store.
    (mark-env! env)
    ;; Flip the marker.
    (set! the-marker! (not the-marker!))
    ;; Sweep away the unmarked cells.
    (sweep!)
))

;; ==================== Expressed Values ==================================

;; Expressed values are Int + Bool + Unit
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (unit-val)
  (proc-val                                                                                          
   (p proc?))  
  (ref-val
   (ref integer?))
)

(define-datatype proc proc?                                                                          
  (procedure                                                                                         
    (params (list-of symbol?))                                                                                  
    (body expression?)                                                                               
    (saved-env environment?)))                                                                       
	       
(define ref-val?
  (lambda (ev)
    (cases expval ev
      [ref-val (ref) #t]
      [else #f])))

(define expval->num 
  (lambda (ev)
    (cases expval ev
      [num-val (num) num]
      [bool-val (bool) (if bool 1 0)]
      [else (raise-exception 'expval->num "Expressed value is not a number or a Boolean: ~s" ev)])))

(define expval->bool
  (lambda (ev)
    (cases expval ev
      [bool-val (bool) bool]
      [num-val (num) (not (= num 0))]
      [else (raise-exception 'expval->bool "Expressed value is not a Boolean or a number: ~s" ev)])))

(define expval->ref
  (lambda (ev)
    (cases expval ev
      [ref-val (ref) ref]
      [else (raise-exception 'expval->ref "Expressed value is not a reference: ~s" ev)])))


(define expval->proc                                                                                 
  (lambda (ev)                                                                                       
    (cases expval ev                                                                                 
      [proc-val (p) p]                                                                               
      [else (raise-exception 'expval->proc "Expressed value is not a procedure: ~s" ev)])))  


(define expval->string
  (lambda (ev)
    (cases expval ev
      [bool-val (bool) (if bool "#true" "#false")]
      [num-val (num) (number->string num)]
      [unit-val () "#unit"]
      [proc-val (p) 
       (cases proc p
	[procedure (vars body env) (string-append "#proc(" (number->string (length vars)) ")")])]
      [ref-val (ref) (string-append "#ref(" (number->string ref) ")") ]
      )))

;; ==================== Evaluater ====================================

(define value-of
  (lambda (prog env)
    (cases program prog
      [a-prog (exp) (cons (value-of-exp exp env) env)]
      [def-prog (var exp) (cons (unit-val) (extend-env var (newref! (value-of-exp exp env)) env))]
      [else (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s" (car prog))])))

(define value-of-exp
  (lambda (exp env)
    (cases expression exp

      ;; Variable Expressions
      [var-exp (var) (deref (apply-env env var))]

      ;; Control Expressions
      [if-exp (exp1 exp2 exp3)
       (if (expval->bool (value-of-exp exp1 env)) (value-of-exp exp2 env) (value-of-exp exp3 env))]
      [let-exp (var exp1 exp2) (value-of-exp exp2 (extend-env var (newref! (value-of-exp exp1 env)) env))]

      ;; Constant Expressions
      [const-true () (bool-val #t)]
      [const-false () (bool-val #f)]
      [const-exp (num) (num-val num)]

      ;; Arithmetic / Logical Operators
      [zero?-exp (exp) (apply-unary-op zero? (value-of-exp exp env) expval->num bool-val)]
      [diff-exp (exp1 exp2) (apply-binary-op - (value-of-exp exp1 env) (value-of-exp exp2 env) expval->num num-val)]
      [plus-exp (exp1 exp2) (apply-binary-op + (value-of-exp exp1 env) (value-of-exp exp2 env) expval->num num-val)]
      [div-exp (exp1 exp2) 
	       (apply-binary-op 
		(lambda (x y) 
		  (if (= y 0) (raise-exception 'value-of-exp "Attempt to divide by zero = ~s/~s." x y) (/ x y)))
		(value-of-exp exp1 env) (value-of-exp exp2 env) expval->num num-val)]

      [times-exp (exp1 exp2) 
       (apply-binary-op 
	* 
	(value-of-exp exp1 env) (value-of-exp exp2 env) 
	expval->num num-val)]

      [less-than-exp (exp1 exp2) 
       (apply-binary-op 
	< 
	(value-of-exp exp1 env) (value-of-exp exp2 env) 
	expval->num bool-val)]

      [equal-exp (exp1 exp2) 
       (apply-binary-op 
	= 
	(value-of-exp exp1 env) (value-of-exp exp2 env) 
	expval->num bool-val)]

      [and-exp (exp1 exp2) 
       (apply-binary-op 
	(lambda (a b) (and a b)) 
	(value-of-exp exp1 env) (value-of-exp exp2 env) 
	expval->bool bool-val)]

      [or-exp (exp1 exp2) 
       (apply-binary-op 
	(lambda (a b) (or a b)) 
	(value-of-exp exp1 env) (value-of-exp exp2 env) 
	expval->bool bool-val)]

      [not-exp (exp) (apply-unary-op not (value-of-exp exp env) expval->num bool-val)]
      
      ;; References 
      [newref-exp (exp) (newref! (value-of-exp exp env))]
      [deref-exp (exp) (deref (value-of-exp exp env))]      
      [setref-exp (exp1 exp2) (setref! (value-of-exp exp1 env) (value-of-exp exp2 env)) (unit-val)] 
      [assign-exp (var exp) (setref! (apply-env env var) (value-of-exp exp env)) (unit-val)]
      
      ;; Procedures
      [proc-exp (vars body) (proc-val (procedure vars body env))]                                      
      [call-exp (exp exps)                                                                          
	(cases proc (expval->proc (value-of-exp exp env))
	 [procedure (params body saved-env)
          (cond
	   [(= (length params) (length exps)) 
	    (let
		([vals (map (lambda (x) (value-of-exp x env)) exps)])
	      (value-of-exp body 
			    (accumulate 
			     (lambda (head acc) (extend-env (car head) (newref! (cdr head)) acc)) 
			     saved-env
			     (reverse (map (lambda (param val) (cons param val)) params vals)))))]
	   [else (raise-exception 
		  'value-of-exp 
		  "Attempt to apply function with inconsistent number of arguments: ~s ~s." exp exps)])])]
      [letrec-exp (p-name p-vars p-body body)                                                         
		  (value-of-exp body (extend-env-rec p-name p-vars p-body env))]     
      
      ;; Printing
      [print-exp (exp) (display (expval->string (value-of-exp exp env))) (unit-val)]
      [newline-exp ()  (newline) (unit-val)]
      
      ;; Blocks
      [block-exp (exps) (accumulate (lambda (exp acc) (value-of-exp exp env)) (unit-val) exps)]
			
      [else (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))

;; ==================== Evaluation Helper Functions ====================

(define all?
  (lambda (p? ls)
    (accumulate (lambda (head acc) (and (p? head) acc)) #t ls)))

(define apply-unary-op
  (lambda (op arg in-acc out-cons)
    (out-cons (op (in-acc arg)))))

(define apply-binary-op
  (lambda (op arg1 arg2 in-acc out-cons)
    (out-cons (op (in-acc arg1) (in-acc arg2)))))

;; ==================== Interpreter ====================================

;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n=== Welcome to the Basic HW 5 Interpreter === \n\n")
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
	  (untrace expval->num expval->bool expval->string expval->proc)]
	 [(equal? code "!debug2")
	  (trace value-of value-of-exp expval->num expval->bool expval->string expval->proc)]
	 [(equal? code "!debug3")
	  (trace value-of value-of-exp expval->num expval->bool expval->string apply-env expval->proc extend-env empty-env)]
	 [(equal? code "!env")
	  (display (env->string env))
	  (newline)]
	 [(equal? code "!store")
	  (display-the-store!)]
	 [(equal? code "!reset-env")
	  (set! env (make-init-env))]
	 [else
	 ;; Parse code, eval expression, and print result.
	  (guard 
	   (e [else (display-exception e)]) ;; Now handles exceptions based on datatypes
	   (let
	       ([abstract-code (parse code)])  ;; Try to parse the input line
	     (let*
		 ([result (value-of abstract-code env)]
		  [val (car result)]
		  [new-env (cdr result)])
	       (display (expval->string val))
	       (set! env new-env)  
	       (newline)
	       )))
	  ;; The garbage collector goes outside the guard so it can clean up after exceptions as well.
	  (garbage-collect! env)
	  ])
	(read-eval-print env)]))))