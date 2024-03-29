;; store.scm

;; =============== Store ====================

;; the-store! is the store!
(define the-store! 'uninitialized)

(define-datatype cell cell?
	(expval-cell
		(val expval?)
        (m boolean?))
	(free-cell
		(ref number?)
		(next number?)))

;; Index of next free cell in store
;; The object at said index will contain a reference to the next
;; free cell's index
;;
;; -1 means no free cell
(define free-list! -1)

;; (empty-store) return an empty Scheme vector representing the empty
;; store.
(define empty-store
	(lambda ()
		(vector)))

;; (initialize-store!) it initializes the-store! to (empty-store)
(define initialize-store!
	(lambda ()
		(set! the-store! (empty-store))))

;; Range from HS #1
(define range 
  (lambda (l u)
    (cond
     [(> l u) '()]
     [else (cons l (range (+ l 1) u))])))

(define no-free?
	(lambda () (= free-list! -1)))

;; (newref! val) takes a value val adds to the the-store!, and returns
;; a ref-val to the added value val.
(define newref!
	(lambda (val)
		(cond 
			;; No free slots in store
			;; We must grow store!
			[(no-free?)
				(let*
					(
					[numfull (vector-length the-store!)] ; Number of items currently in our store
					[newlen (+ 1 (* 2 numfull))] ; New length of the store
					[lastIndex (- newlen 1)] ; Index of the last item in the new store
					[newstore (make-vector newlen (free-cell lastIndex -1))] ; New store vector
					)

					; Copy old values to new store
					(for-each
						(lambda (i c) 
							(vector-set! newstore i c)) 
						(range 0 (- numfull 1))
						(vector->list the-store!))

					; Insert new val at numfull position
					(vector-set! newstore numfull (expval-cell val #f))

					; Update new free cells
					; (numfull + 1 through newlen -2)
					(for-each
						(lambda (i)
							(vector-set! newstore i (free-cell i (+ i 1))))
						(range (+ numfull 1) (- lastIndex 1)))

					; Update free list
					(set! free-list!
							(if (>= (+ numfull 1) newlen)
								-1
								(+ numfull 1)))						    

					; Update store
					(set! the-store! newstore)
			
					; Return reference to newly inserted item
					(ref-val numfull)
				)]
				
			;; We have a free cell, stick val there!
			[else
				; First look up index of free-list to get free-cell
				(let ([c (vector-ref the-store! free-list!)])
					(cases cell c
						[free-cell (ref next)
				
							; Set val to current index of free cell
							(vector-set! the-store! ref (expval-cell val #f))

							; Update free list to next index pointer
							(set! free-list! next)

							; Return reference to newly inserted item
							(ref-val ref)]
						[else (raise-exception 'newref "Something went wrong! Couldn't find room for a new reference!")]
					))] 
		)))

;; (deref ev) expects that ev is a reference (ref-val ref), and
;; returns the value of associated with ref in the store.
(define deref
	(lambda (ev)
		(let 
			([ref (expval->ref ev)])
			(cases cell (vector-ref the-store! ref)
				[expval-cell (val m) val]
				[else (raise-exception 'deref "Attempted to deref null reference!")]))
		))

;; (setref! ev val) expects that ev is a reference (ref-val ref), and
;; it sets the reference ref to val in the the-store!
(define setref!
	(lambda (ev val)
		(let
			([ref (expval->ref ev)])
			(vector-set! the-store! ref (expval-cell val #f)))))


;; (free! i)
;; deletes the value at the corresponding index i in the store
(define free!
	(lambda (i)
			(if (or (< i 0) (>= i (vector-length the-store!)))
				(raise-exception 'free! "Invalid store index!"))
			(vector-set! the-store! i (free-cell i free-list!))
			(set! free-list! i)))


;; Collects garbage from the store using a Mark-sweep algorithm
(define collect
  (lambda (root)
    (mark root)
    (sweep)))

(define expval-cell->m
	(lambda (c) 
		(cases cell c
			[expval-cell (val m) m]
			[else (raise-exception 'expval-cell->m "(m) Cell is not an expval-cell!")])))

(define expval-cell->val
	(lambda (c) 
		(cases cell c
			[expval-cell (val m) val]
			[else (raise-exception 'expval-cell->val "(val) Cell is not an expval-cell!")])))

(define mark
  (lambda (e)
    (cond
        [(environment? e)
         (cases environment e
                [empty-env () 0]
                [extend-env (var val env)
                	(let* (
                		[ref (expval->ref val)]
                		[c (vector-ref the-store! ref)]
	                	[marked (expval-cell->m c)])
                		(if (not marked)
                			(let ([ev (expval-cell->val c)])
	                                  (vector-set! the-store! ref (expval-cell ev #t)) 
	                                  (mark ev)))
                		(mark env))]
                [extend-env-rec (f-name f-param f-body env) (mark env)])]
        [(expval? e)
         	(cases expval e
         		[proc-val (param body env) (mark env)]
         		[list-val (ls) (for-each mark ls)]
         		[ref-val (ref) 
         			(let* (
         				[c (vector-ref the-store! ref)]
         				[marked (expval-cell->m c)])
	             		(if (not marked)
	            			(let ([ev (expval-cell->val c)])
	                                  (vector-set! the-store! ref (expval-cell ev #t)) 
	                                  (mark ev))))]
         		[else 0])])
))

(define sweep
  (lambda ()
    (for-each (lambda (i c)
    	(cases cell c
    		[expval-cell (val m)
					(if m
                         (vector-set! the-store! i (expval-cell val #f))
                         (free! i))]
			[else 0])) 
         (range 0 (- (vector-length the-store!) 1)) 
         (vector->list the-store!))))

(define print-store!
	(lambda ()
		 (for-each (lambda (i c)
		 	(display i)
		 	(display " : ")
		 	(display c)
		 	(newline))
         (range 0 (- (vector-length the-store!) 1)) 
         (vector->list the-store!))))


;; =============== GC Test Programs ====================
;;
;; > def! a = 5
;;
;; -----------------------------------------------------
;;
;; > def! b = let x = 10 in 15
;;
;; -----------------------------------------------------
;;
;; > def! p = let a = 5 in let b = 6 in proc () 10
;;
;; -----------------------------------------------------
;;
;; > def! g = let y = 42 in let f = proc(x) y in f
;; > (g 12)
;;
;; -----------------------------------------------------
;;
;; > def! r = let r1 = newref!(0) in let r2 = newref!(r1) in { setref!(r1,r2)  r1 }
;; > deref(r)
;; > deref(deref(r))
;;
;; -----------------------------------------------------
;;
;; > def! p = newref!(0)
;; > setref!(p,newref!(1))
;; > setref!(deref(p),newref!(2))
;;
;; -----------------------------------------------------
;;
