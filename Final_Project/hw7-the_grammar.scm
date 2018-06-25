;; the_grammar.scm

;; ================ Parser Definitions ==================================

;; This defines the translation from the concrete syntax to the abstract syntax
(define the-grammar
  '((program                        ;; <Program> ::= 
     (expression)                   ;;   Concrete    <Expression>
     a-prog)                        ;;   Abstract    (a-prog exp)
    
    (expression                     ;; <Expression> ::= 
     (number)                       ;;   Concrete       <Number> 
     const-exp)                     ;;   Abstract       (const-exp num)
    
    (expression                     ;; <Expression> ::= 
     ("zero?(" expression ")")      ;;   Concrete       zero?(<Expression>)
     zero?-exp)                     ;;   Abstract       (zero?-exp exp)
    
    (expression                                             ;; <Expression> ::= 
     ("if" expression "then" expression "else" expression)  ;;   Concrete       if <Expression> then <Expression> else <Expression>
     if-exp)                                                ;;   Abstract       (if-exp exp1 exp2 exp3)
        
    (expression                     ;; <Expression> ::= 
     (identifier)                   ;;   Concrete       <Identifier>
     var-exp)                       ;;   Abstract       (var-exp var)
    
    (expression                                          ;; <Expression> ::= 
     ("let" identifier "=" expression "in" expression)   ;;   Concrete       let <Identifier> = <Expression> in <Expression>
     let-exp)                                            ;;   Abstract       (let-exp var exp1 exp2)

    ;; ============== PROC Definitions below ========================

    (expression                                     ;; <Expression> ::=
     ("proc" "(" (arbno identifier) ")" expression) ;;   Concrete       proc (<Identifier>*) <Expression>
     proc-exp)                                      ;;   Abstract       (proc-exp vars exp) 
    
    (expression                                    ;; <Expression> ::= 
     ("(" expression (arbno expression) ")")       ;;   Concrete       (<Expression> <Expression>*)
     call-exp)                                     ;;   Abstract       (call-exp exp exps)

    ;; ============== LETREC Definitions below ========================
    
    (expression                                    ;; <Expression> ::=                                 
     ("letrec" identifier "(" (arbno identifier)   ;;   Concrete       letrec <Identifier>(<Identifier>*) = <Expression> in <Expression>
      ")" "=" expression "in" expression)          ;;   Abstract       (letrec-exp p-name p-vars p-body body)
     letrec-exp)

    ;; ============== HW 5 Definitions below ========================

    (program                               ;; <Program> ::= 
     ("def!" identifier "=" expression)    ;;  Concrete     def! <Identifier> = <Expression>
     def-prog)                             ;;  Abstract     (def-prog var exp)
    
    (expression                            ;; <Expression> ::= 
     ("#true")                             ;;   Concrete       #true
     const-true-exp)                       ;;   Abstract       (const-true-exp)
    
    (expression                            ;; <Expression> ::=
     ("#false")                            ;;   Concrete       #false
     const-false-exp)                      ;;   Abstract       (const-false-exp)
     
    (expression                            ;; <Expression> ::= 
     ("*(" expression "," expression ")")  ;;   Concrete       *(<Expression>,<Expression>)
     times-exp)                            ;;   Abstract       (times-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("/(" expression "," expression ")")  ;;   Concrete       /(<Expression>,<Expression>)
     div-exp)                              ;;   Abstract       (div-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("-(" expression "," expression ")")  ;;   Concrete       -(<Expression>,<Expression>)
     diff-exp)                             ;;   Abstract       (diff-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("+(" expression "," expression ")")  ;;   Concrete       +(<Expression>,<Expression>)
     plus-exp)                             ;;   Abstract       (plus-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("=(" expression "," expression ")")  ;;   Concrete       =(<Expression>,<Expression>)
     equal-exp)                            ;;   Abstract       (equal-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("<(" expression "," expression ")")  ;;   Concrete       <(<Expression>,<Expression>)
     less-than-exp)                        ;;   Abstract       (less-than-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("&(" expression "," expression ")")  ;;   Concrete       &(<Expression>,<Expression>)
     and-exp)                              ;;   Abstract       (and-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("|(" expression "," expression ")")  ;;   Concrete       |(<Expression>,<Expression>)
     or-exp)                               ;;   Abstract       (or-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("~(" expression ")")                 ;;   Concrete       ~(<Expression>)
     not-exp)                              ;;   Abstract       (not-exp exp)

    (expression                               ;; <Expression> ::=
     ("cons(" expression "," expression ")")  ;;   Concrete       cons(<Expression>,<Expression>)
     cons-exp)                                ;;   Abstract       (cons-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::=
     ("car(" expression ")")               ;;   Concrete       car(<Expression>)
     car-exp)                              ;;   Abstract       (car-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("cdr(" expression ")")               ;;   Concrete       cdr(<Expression>)
     cdr-exp)                              ;;   Abstract       (cdr-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("null?(" expression ")")             ;;   Concrete       null?(<Expression>)
     null?-exp)                            ;;   Abstract       (null?-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("emptylist")                         ;;   Concrete       emptylist
     emptylist-exp)                        ;;   Abstract       (emptylist-exp)

    (expression                            ;; <Expression> ::=
     ("newref!(" expression ")")           ;;   Concrete       newref!(<Expression>)
     newref-exp)                           ;;   Abstract       (newref-exp exp)
    
    (expression                            ;; <Expression> ::=
     ("deref(" expression ")")             ;;   Concrete       deref(<Expression>)
     deref-exp)                            ;;   Abstract       (deref-exp exp)
    
    (expression                                 ;; <Expression> ::=
     ("setref!(" expression "," expression ")") ;;   Concrete  setref!(<Expression>,<Expression>)
     setref-exp)                                ;;   Abstract  (setref-exp exp1 exp2)

    (expression                            ;; <Expression> ::=
     ("set" identifier "=" expression )    ;;    Concrete      set <Identifier> = <Expression>
     assign-exp)                           ;;    Abstract      (assign-exp var exp)

    ;; ============== HW 6 Definitions below ========================

    (expression                                           ;; <Expression> ::=
     ("{" (arbno expression) "}")                         ;;   Concrete       {<Expression>*}
     block-exp)                                           ;;   Abstract       (block-exp exps)
    
    (expression                                           ;; <Expression> ::=
     ("print!" "(" expression ")")                        ;;   Concrete       print!(<Expression>)
     print-exp)                                           ;;   Abstract       (print-exp exp)                         

    (expression                                           ;; <Expression> ::=
     ("newline!")                                         ;;   Concrete       newline!
     newline-exp)                                         ;;   Abstract       (newline-exp) 
    
    ))

(load "lex-scan-parse.scm")

;; ==================== Parser ======================================
(define parse
  (lambda (str)
    (scan&parse str)))
