;; hw4-soln-sllgen.ss

;; (load "grader.ss")

;; This define the lexical specification of the scanner
(define the-lexical-spec
  '((whitespace (whitespace) skip)                   ;; PL ignores whitespace
    (comment ("%" (arbno (not #\newline))) skip)     ;; PL ignores comments written by %
    (identifier                                          
     (letter (arbno (or letter digit "_" "-" "?")))  ;; PL has ids which begin with a letter followed by 
     symbol)                                         ;; any number of letters digits _ - ? 
    (number (digit (arbno digit)) number)            ;; PL has positive numbers
    (number ("-" digit (arbno digit)) number)        ;; PL has negative numbers
    ))

;; This defines the translation from the concrete syntax to the abstract syntax
(define the-grammar
    '((prefix-list                          ;; (prefix-exp)
       ("(" prefix-exp ")") 
       list-exp)

      (prefix-exp                           ;; Int
       (number) 
       const-exp)

      (prefix-exp                           ;; - prefix-exp prefix-exp
       ("-" prefix-exp prefix-exp)
       diff-exp)
      ))
  
;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

;; This command makes the abstract grammar from the description in the-grammar
;; With this you don't need to use define-datatypes
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

;; Use to translate from concrete syntax to abstract syntax
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
;;;;;;;;;;;;;; end sllgen boilerplate ;;;;;;;;;;;;;

;; The SLLGEN trivializes parse-prefix which now takes a string rather
;; than a Scheme expression in the concrete syntax.
(define parse-prefix
  (lambda (str)
    (scan&parse str)))

;; Test cases
;; (add-batch-tests! "parse-prefix" '(
;; (parse-prefix "(2)") => (list-exp (const-exp 2))

;; (parse-prefix "(- - 3 2 - 4 - 12 7)") 
;; => 
;; (list-exp (diff-exp (diff-exp (const-exp 3) (const-exp 2)) (diff-exp (const-exp 4) (diff-exp (const-exp 12) (const-exp 7)))))

;; (parse-prefix "(- 2 3)")
;; =>
;; (list-exp (diff-exp (const-exp 2) (const-exp 3)))
;; ))

;; (unparse-prefix pf-exp) recurses through abstract prefix expression
;; using cases to translate back into concrete syntax. Follows the
;; grammar.  Now we translate back to a string, have to deal with the
;; extra list case too.
(define unparse-prefix
  (lambda (pfl-exp)
    (cases prefix-list pfl-exp
      [list-exp (pf-exp)  (string-append "(" (unparse-prefix* pf-exp) ")")]
      [else (eopl:error 'parse "Invalid abstract syntax. ~s~%" pfl-exp)])))

(define unparse-prefix*
  (lambda (pf-exp)
    (cases prefix-exp pf-exp
      [const-exp (num) (number->string num)]
      [diff-exp (rand1 rand2) (string-append "- " (unparse-prefix* rand1) " " (unparse-prefix* rand2))]
      [else (eopl:error 'parse "Invalid abstract syntax. ~s~%" pf-exp)])))

;; (add-batch-tests! "unparse-prefix" '(
;; (unparse-prefix (list-exp (const-exp 2))) => "(2)"
;; (unparse-prefix 
;;   (list-exp (diff-exp 
;;    (diff-exp (const-exp 3) (const-exp 2)) (diff-exp (const-exp 4) (diff-exp (const-exp 12) (const-exp 7))))))
;; => 
;; "(- - 3 2 - 4 - 12 7)"
;; (unparse-prefix (list-exp (diff-exp (const-exp 2) (const-exp 3))))
;; =>
;; "(- 2 3)"
;; ))

;; (eval-prefix pf-exp) recurses through abstract prefix expression
;; using cases to evaluate it. Follows the grammar.
(define eval-prefix
  (lambda (pfl-exp)
    (cases prefix-list pfl-exp
      [list-exp (pf-exp)  (eval-prefix* pf-exp)]
      [else (eopl:error 'parse "Invalid abstract syntax. ~s~%" pfl-exp)])))
  
(define eval-prefix*
  (lambda (pf-exp)
    (cases prefix-exp pf-exp
      [const-exp (num) num]
      [diff-exp (rand1 rand2) (- (eval-prefix* rand1) (eval-prefix* rand2))]
      [else (eopl:error 'parse "Invalid abstract syntax. ~s~%" pf-exp)])))

;; (add-batch-tests! "eval-prefix" '(
;; (eval-prefix (list-exp (const-exp 2))) => 2

;; (eval-prefix 
;;  (list-exp (diff-exp 
;; 	    (diff-exp (const-exp 3) (const-exp 2)) (diff-exp (const-exp 4) (diff-exp (const-exp 12) (const-exp 7))))))
;; => 
;; 2

;; (eval-prefix (list-exp (diff-exp (const-exp 2) (const-exp 3))))
;; =>
;; -1

;; ))




;;(run-all-tests!)
