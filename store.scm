;; store.scm

;; =============== Store ====================

;; the-store! is the store!
(define the-store! 'uninitialized)

(define-datatype cell cell?
	(expval-cell
		(val expval?))
	(free-cell
		(ref number?)
		(next number?))
)

;; Reference to next free cell in store
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

;; (newref! val) takes a value val adds to the the-store!, and returns
;; a ref-val to the added value val.
(define newref!
	(lambda (val)
		(cond 

			;; No free slots in store
			;; We must grow store!
			[(= free-list! -1) 

				(let*
					([numfull (vector-length the-store!)]
					[newlen (+ 1 (* 2 numfull))]
					[newstore (make-vector newlen (free-cell (- newlen 1) -1))]
					[i 0])

					; Copy old values to new store
					(vector-for-each 
						(lambda (head) 
							(vector-set! newstore i head)
							(set! i (+ 1 i)) ; Side effects oh no!
						) 
						the-store!)

					; Insert new val
					(vector-set! newstore numfull (expval-cell val))

					; Update new frees cells
					; numfull + 1 to newlen - 2
					(fold-right 
						(lambda (i acc)
							(vector-set! newstore i (free-cell i (+ i 1)))
						)
						#f
						(range (+ numfull 1) (- newlen 2)))

					;(display "newlen ")
					;(display newlen)
					;(display "\n")
					;(display "numfull ")
					;(display numfull)
					;(display "\n")

					; Update free list
					(set! free-list
							(cond 
								[(= newlen 1) -1]
								[else (+ numfull 2)])) ; ????							    

					; Update store
					(set! the-store! newstore)
			
					(ref-val numfull)
				)]
				
			;; We have a free cell, stick val there!
			[else
				(let ([c (vector-ref the-store! free-list!)])
					(cases cell c
						[free-cell (ref next)
							(set! free-list! next)
							(vector-set! the-store! ref (expval-cell val))
							(ref-val ref)]
					))]

		))



		;(set! the-store! (append the-store! (list val)))
		;(ref-val (- (length the-store!) 1))
)

;; (deref ev) expects that ev is a reference (ref-val ref), and
;; returns the value of associated with ref in the store.
(define deref
	(lambda (ev)
		(let 
			([ref (expval->ref ev)])
			(cases cell (vector-ref the-store! ref)
				[expval-cell (val) val]
				[else (raise-exception 'deref "Attempted to deref null reference!")]))
		))

;; (setref! ev val) expects that ev is a reference (ref-val ref), and
;; it sets the reference ref to val in the the-store!
(define setref!
	(lambda (ev val)
		(let
			([ref (expval->ref ev)])
			(vector-set! the-store! ref (expval-cell val)))))
