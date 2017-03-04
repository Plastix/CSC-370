;; environment.scm

;; =============== Environment Definition =============================


;; This is an implementation of the var-val pair list representation
;; of an environment, we wrote earlier.  I translated the
;; representation into a define-datatype so we get the constructors
;; and type checking predicate for free, and can use cases to process.

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val ref-val?)
   (env environment?))
  (extend-env-rec
   (f-name symbol?)
   (f-params (list-of symbol?))
   (f-body expression?)
   (saved-env environment?))
  )


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
			(newref! (proc-val p-vars p-body env))]
		       [else (apply-env env* target-var)])])))


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
		       " = "  (expval->string val) 
		       (env->string* env*) "]")]
      [extend-env-rec (p-name p-var p-body env*)
				(string-append "[" (symbol->string p-name) " =  #recproc" (env->string* env*) "]")]
			)))

(define env->string*
  (lambda (env)
    (cases environment env
      [empty-env () ""]
      [extend-env (var val env*) 
				(string-append ", " (symbol->string var) 
					" = " (expval->string val) 
					(env->string* env*))]
      [extend-env-rec (p-name p-var p-body env*)
				(string-append ", " (symbol->string p-name) " =  #recproc" (env->string* env*))]
			)))
