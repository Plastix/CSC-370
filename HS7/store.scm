;; store.scm

;; =============== Store ====================

;; the-store! is the store!
(define the-store! 'uninitialized)

;; (empty-store) return an empty Scheme list representing the empty
;; store.
(define empty-store
	(lambda ()
		'() ))

;; (initialize-store!) it initializes the-store! to (empty-store)
(define initialize-store!
	(lambda ()
		(set! the-store! (empty-store))))

;; (newref! val) takes a value val adds to the the-store!, and returns
;; a ref-val to the added value val.
(define newref!
	(lambda (val)
		(set! the-store! (append the-store! (list val)))
		(ref-val (- (length the-store!) 1))
))

;; (deref ev) expects that ev is a reference (ref-val ref), and
;; returns the value of associated with ref in the store.
(define deref
	(lambda (ev)
		(let 
			([ref (expval->ref ev)])
			(list-ref the-store! ref))))

;; (setref! ev val) expects that ev is a reference (ref-val ref), and
;; it sets the reference ref to val in the the-store!
(define setref!
	(lambda (ev val)
		(let
			([ref (expval->ref ev)])
			(set! the-store! (setref!* the-store! ref val)))))

;; (setref!* store ref val) should return the store where var has been set to val.
;; ref is an integer index into the store.  0 is the first element of the store and
;; (- (length store) 1) is the index of the last element of the store.
(define setref!*
	(lambda (store ref val)
		(cond
			[(= 0 ref) (cons val (cdr store))]
			[else (cons (car store) (setref!* (cdr store) (- ref 1) val))])))
