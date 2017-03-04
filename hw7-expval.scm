;; expval.scm

;; ==================== Expressed Values ==================================

;; Expressed values are Int + Bool + Unit
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (unit-val)
  (proc-val
   (param (list-of symbol?))
   (body expression?)
   (saved-env environment?))
  (list-val
   (ls list?))
  (ref-val
   (ref integer?))
)

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

(define expval->proc-body
	(lambda (ev)
		(cases expval ev
			[proc-val (param body env) body]
			[else (raise-exception 'expval->num "Expressed value is not a procedure: ~s" ev)])))

(define expval->proc-params
	(lambda (ev)
		(cases expval ev
			[proc-val (param body env) param]
			[else (raise-exception 'expval->num "Expressed value is not a procedure: ~s" ev)])))

(define expval->proc-saved-env
	(lambda (ev)
		(cases expval ev
			[proc-val (param body env) env]
			[else (raise-exception 'expval->num "Expressed value is not a procedure: ~s" ev)])))

(define expval->list
  (lambda (ev)
    (cases expval ev
      [list-val (ls) ls]
      [else (raise-exception 'expval->list "Expressed value is not a list: ~s" ev)])))

(define expval->ref
	(lambda (ev)
		(cases expval ev
			[ref-val (ref) ref]
			[else (raise-exception 'expval->ref "Expressed value is not a reference: ~s" ev)])))

(define expval->string
  (lambda (ev)
    (cases expval ev
      [bool-val (bool) (if bool "#true" "#false")]
      [num-val (num) (number->string num)]
      [list-val (ls)
				(string-append
					"("
					(fold-left
						string-append
						""
						(map
							(lambda (ev) (string-append (expval->string ev) " "))
							ls))
					")")]
      [proc-val (param body saved-env) "#proc"]
      [unit-val () ""]
			[ref-val (ref) (string-append "#ref(" (number->string ref) ")") ]
			)))
      
  
