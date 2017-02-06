;; CSC 370 HS 5
;; Aidan Pieper
;; 2/15/17
;; LET-interp.scm

(load "helpers.scm")

;; ================ Parser Definitions ==================================

;; This defines the translation from the concrete syntax to the abstract syntax.
;; Whenever  you add to or modify the concrete or abstract syntax the specification
;; below must be updated.

(define the-grammar
  '((program                        ;; <Program> ::= 
      (expression)                   ;;   Concrete    <Expression>
      a-prog)                        ;;   Abstract    (a-prog exp)

    (program
      ("!def" identifier "=" expression)
      def-prog)

    (expression                     ;; <Expression> ::= 
      (number)                       ;;   Concrete       <Number> 
      const-exp)                     ;;   Abstract       (const-exp num)

    (expression                            ;; <Expression> ::= 
      ("-(" expression "," expression ")")  ;;   Concrete       -(<Expression>,<Expression>)
      diff-exp)                             ;;   Abstract       (diff-exp exp1 exp2)

    (expression
      ("+(" expression "," expression ")")
      sum-exp)

    (expression
      ("*(" expression "," expression ")")
      mult-exp)

    (expression
      ("/(" expression "," expression ")")
      div-exp)

    (expression
      ("<(" expression "," expression ")")
      less-exp)

    (expression
      ("<=(" expression "," expression ")")
      leq-exp)

    (expression
      ("=(" expression "," expression ")")
      eq-exp)

    (expression                     ;; <Expression> ::= 
      ("zero?(" expression ")")      ;;   Concrete       zero?(<Expression>)
      zero?-exp)                     ;;   Abstract       (zero?-exp exp)

    (expression                                             ;; <Expression> ::= 
      ("if" expression "then" expression "else" expression)  ;;   Concrete       if <Expression> then <Expression> else <Expression>
      if-exp)                                                ;;   Abstract       (if-exp exp1 exp2 exp3)

    (expression           ;; var
      (identifier)
      var-exp)

    (expression
      ("let" identifier "=" expression "in" expression)
      let-exp)

    (expression
      ("#true")
      const-true-exp)

    (expression
      ("#false")
      const-false-exp)

    (expression
      ("&(" expression "," expression ")")
      and-exp)

    (expression
      ("|(" expression "," expression ")")
      or-exp)

    (expression
      ("!(" expression ")")
      not-exp)
    ))

;; Sets up the parser using the above concrete <-> abstract grammars.
;; Defines a function call parse that takes a string in the concrete
;; syntax and returns the corresponding abstract syntax tree. You must
;; have defined the-grammar first.
(load "lex-scan-parse.scm")


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
                   (env environment?)))

;; (apply-env env target-var) s to figure out the maping of target-var
;; in the environment env.
(define apply-env ; Env x Var -> SType
  (lambda (env target-var)
    (cases environment env
           [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]
           [extend-env (var val env*) 
                       (cond
                         [(equal? var target-var) val]
                         [else (apply-env env* target-var)])])))

;; Initializes an environment with a few basic constants
(define make-init-env
  (lambda ()
    (extend-env
      'e
      (num-val 2.71828)
      (extend-env
        'pi
        (num-val 3.14159)
        (empty-env)))))

(define print-env!
  (lambda (env)
    (display "[")
    (print-env!* env)
    (display "]\n")))

(define print-env!*
  (lambda (env)
    (cases environment env
           [empty-env () ""]
           [extend-env (var val env*)
                       (display var)
                       (display " = ")
                       (display (expval->string val))
                       (if (not (equal? env* (empty-env)))
                         (display ", "))
                       (print-env!* env*)])))

;; ==================== Expressed Values ==================================

(define-datatype expval expval?
                 (num-val
                   (num number?))
                 (bool-val
                   (b boolean?))
                 (unit-val))

(define expval->num
  (lambda (ev)
    (cases expval ev
           [num-val (num) num]
           [bool-val (b) (if b 1 0)]
           [else (raise-exception 'expval->num "Expressed value is not a number: ~s" ev)])))

(define expval->bool
  (lambda (ev)
    (cases expval ev
           [bool-val (b) b]
           [num-val (num) (if (= num 0) #f #t)]
           [else (raise-exception 'expval->bool "Expressed value is not a Boolean: ~s" ev)])))

(define expval->string
  (lambda (ev)
    (cases expval ev
           [bool-val (b) (if b "#true" "#false")]
           [num-val (num) (number->string num)]
           [unit-val () ""]
           )))


;; ==================== Evaluater =========================================
(define value-of-prog
  (lambda (prog env)
    (cases program prog
           [a-prog (exp) (list (value-of-exp exp env) env)]
           [def-prog (var exp) (let ([ev (value-of-exp exp env)])
                                 (list (unit-val) 
                                       (extend-env var ev env)))]
           [else (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s"  (car prog))])))

(define binary-op-num
  (lambda (op env exp1 exp2)
    (let
      ([ev1 (value-of-exp exp1 env)]
       [ev2 (value-of-exp exp2 env)])
      (num-val (op (expval->num ev1)
                   (expval->num ev2))))))   

(define value-of-exp
  (lambda (exp env)
    (cases expression exp
           [const-exp (num) (num-val num)]
           [diff-exp (rand1 rand2) (binary-op-num - env rand1 rand2)]
           [sum-exp (rand1 rand2) (binary-op-num + env rand1 rand2)]
           [mult-exp (rand1 rand2) (binary-op-num * env rand1 rand2)]
           [div-exp (rand1 rand2)
                    (let ([val2 (expval->num (value-of-exp rand2 env))])
                      (if (zero? val2)
                        (raise-exception 'value-of-exp "Cannot divide by 0!")
                        (binary-op-num / env rand1 rand2)))]
           [less-exp (rand1 rand2) (bool-val (< (expval->num (value-of-exp rand1 env))
                                                (expval->num (value-of-exp rand2 env))))]
           [leq-exp (rand1 rand2) (bool-val (<= (expval->num (value-of-exp rand1 env))
                                                (expval->num (value-of-exp rand2 env))))]
           [eq-exp (rand1 rand2) (bool-val (= (expval->num (value-of-exp rand1 env))
                                              (expval->num (value-of-exp rand2 env))))]
           [zero?-exp (exp1) (bool-val (= (expval->num (value-of-exp exp1 env)) 0))]
           [if-exp (exp1 exp2 exp3) 
                   (let
                     ([val1 (expval->bool (value-of-exp exp1 env))])
                     (if val1 (value-of-exp exp2 env) (value-of-exp exp3 env)))]
           [var-exp (var) (apply-env env var)]
           [let-exp (var exp1 exp2)
                    (let 
                      ([val1 (value-of-exp exp1 env)])
                      (value-of-exp exp2 (extend-env var val1 env)))]
           [const-true-exp () (bool-val #t)]
           [const-false-exp () (bool-val #f)]
           [and-exp (exp1 exp2)
                    (let ([ev1 (value-of-exp exp1 env)]
                          [ev2 (value-of-exp exp2 env)])
                      (bool-val (and (expval->bool ev1)
                                     (expval->bool ev2))))]
           [or-exp (exp1 exp2)
                   (let ([ev1 (value-of-exp exp1 env)]
                         [ev2 (value-of-exp exp2 env)])
                     (bool-val (or (expval->bool ev1)
                                   (expval->bool ev2))))]
           [not-exp (exp1) 
                    (let ([ev1 (value-of-exp exp1 env)])
                      (bool-val (not (expval->bool ev1))))]
           [else (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))


;; =================== Interpreter =========================================
;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n=== A Simple Interpreter === \n\n")
      (read-eval-print (make-init-env)))))

;; (get-input-string) -- Reads a line from the interactive input
;; port.  Ignores zero length strings.
(define get-input-string
  (lambda ()
    (let ([str (get-line (current-input-port))])
      (if (= (string-length str) 0) 
        (get-input-string)
        str))))

(define debug0
  (lambda ()
    (trace value-of-prog)
    (trace value-of-exp)
    ))

(define debug1
  (lambda ()
    (debug0)
    (trace expval->num)
    (trace expval->bool)
    (trace expval->string)))

(define debug2
  (lambda ()
    (untrace value-of-prog)
    (untrace value-of-exp)
    (untrace expval->num)
    (untrace expval->bool)
    (untrace expval->string)))

;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Display an interpreter prompt
    (display "==> ")
    ;; Read a line user input
    (let ([concrete-code (get-input-string)])
      (cond
        [(equal? concrete-code "!quit")  
         (display "Goodbye!")  ;; Quit if 'quit entered.
         (newline)]
        [(equal? concrete-code "!debug0") 
         (debug0) 
         (read-eval-print env)]
        [(equal? concrete-code "!debug1") 
         (debug1)
         (read-eval-print env)]
        [(equal? concrete-code "!debug2") 
         (debug2)
         (read-eval-print env)]
        [(equal? concrete-code "!env")
         (print-env! env)
         (read-eval-print env)]
        [(equal? concrete-code "!reset-env") (read-eval-print (make-init-env))]
        [else
          (guard
            (ex
              [else
                (display "PARSE ERROR: \n")
                (display-exception ex)])
            ;; Parse code, eval expression, and print result.
            (let
              ([abstract-code (parse concrete-code)])
              (guard
                (ex
                  [else
                    (display "RUNTIME ERROR: \n")
                    (display-exception ex)])
                (let* ([val (value-of-prog abstract-code env)]
                       [newenv (cadr val)]
                       [return (car val)]) 
                  (set! env newenv)
                  (display (expval->string return))
                  (newline))
                )))
          ;; "Loop".  Notice it is tail recursive.
          (read-eval-print env)]))))

