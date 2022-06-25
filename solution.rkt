#lang plait

(module+ test
  (print-only-errors #t))

(define-type Exp
  (varE [x : Symbol])
  (numE [n : Number])
  (binopE [e1 : Exp] [op : (Value Value -> Value)] [e2 : Exp])
  (ifzE [e1 : Exp] [e2 : Exp] [e3 : Exp])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (funE [name : Symbol] [xs : (Listof Symbol)] [e : Exp])
  (appE [name : Symbol] [es : (Listof Exp)])
  (defineE [es : (Listof Exp)] [e : Exp]))


(define (parse [s : S-Exp]) : Exp
  (local [(define (parse-body [s : S-Exp]) : Exp
            (cond
              [(s-exp-match? `SYMBOL s)
               (varE (s-exp->symbol s))]
              [(s-exp-match? `NUMBER s)
               (numE (s-exp->number s))]
              [(s-exp-match? `{ANY SYMBOL ANY} s)
               (binopE
                (parse-body (first (s-exp->list s)))
                (parse-op (s-exp->symbol (second (s-exp->list s))))
                (parse-body (third (s-exp->list s))))]
              [(s-exp-match? `{ifz ANY then ANY else ANY} s)
               (ifzE
                (parse-body (second (s-exp->list s)))
                (parse-body (fourth (s-exp->list s)))
                (parse-body (list-ref (s-exp->list s) 5)))]
              [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
               (letE
                (s-exp->symbol (second (s-exp->list s)))
                (parse-body (fourth (s-exp->list s)))
                (parse-body (list-ref (s-exp->list s) 5)))]
              [(s-exp-match? `{SYMBOL {ANY ...}} s)
               (appE
                (s-exp->symbol (first (s-exp->list s)))
                (map parse-body (s-exp->list (second (s-exp->list s)))))]
              [else (error 'parse (string-append "Syntax error: Expression other than that available in the body detected: " (to-string s)))]))
          
          (define (parse-fun [s : S-Exp]) : Exp
            (cond
              [(s-exp-match? `{fun SYMBOL {SYMBOL ...} = ANY} s)
               (funE
                (s-exp->symbol (second (s-exp->list s)))
                (map s-exp->symbol (s-exp->list (third (s-exp->list s))))
                (parse-body (list-ref (s-exp->list s) 4)))]
              [else (error 'parse (string-append "Syntax error: An expression other than function is detected: " (to-string s)))]))]
    
    (if (s-exp-match? `{define {ANY ...} for ANY} s)
        (defineE
          (map parse-fun (s-exp->list (second (s-exp->list s))))
          (parse-body (fourth (s-exp->list s))))
        (error 'parse (string-append "Syntax error: An expression other than define is detected" (to-string s))))))

(define prim-ops '(+ - * <=))

(define (parse-op [op : Symbol])
  (if (member op prim-ops)
      (cond
        [(eq? op '+) +]
        [(eq? op '-) -]
        [(eq? op '*) *]
        [(eq? op '<=) (lambda (x y) (if (<= x y) 0 42))]) 
      (error 'parse (string-append "Unknown operator: " (to-string op)))))


(define-type-alias Value Number)

(define-type Binding 
  (bind  [name : Symbol]
         [val : Value])
  (bindF [name : Symbol]
         [param : (Listof Symbol)]
         [body : Exp]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x v) env))

(define (extend-env-fun [env : Env] [x : Symbol] [xs : (Listof Symbol)]  [f : Exp]) : Env
  (cons (bindF x xs f) env))

(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup (string-append "Unbound variable: " (to-string n)))]
    [(cons b rst-env)
     (type-case Binding b
       [(bind name val)
        (if (eq? n name)
            val
            (lookup-env n rst-env))]
       [else (lookup-env n rst-env)])]))

(define (lookup-env-fun [n : Symbol] [env : Env]) : ((Listof Symbol) * Exp)
  (type-case (Listof Binding) env
    [empty (error 'lookup (string-append "Undefined function: " (to-string n)))]
    [(cons b rst-env)
     (type-case Binding b
       [(bindF name xs fun)
        (if (eq? n name)
            (pair xs fun)
            (lookup-env-fun n rst-env))]
       [else (lookup-env-fun n rst-env)])]))


(define (apply [xs : (Listof Symbol)] [vs : (Listof Value)] [env : Env]) : Env
  (type-case (Listof Symbol) xs
    [(cons x xs)
     (apply xs (rest vs) (extend-env env x (first vs)))]
    [empty (type-case (Listof Value) vs
             [(cons x xs) (error 'apply "Incorrect number of parameters and arguments")]
             [empty        env])]))


(define (eval [e : Exp] [env : Env]) : Value
  (local [(define global-env env)
          
          (define (eval-body [e : Exp] [env : Env]) : Value
            (type-case Exp e
              [(numE n) n]
              [(varE x)
               (lookup-env x env)]
              [(binopE e1 op e2)
               (op (eval-body e1 env) (eval-body e2 env))]
              [(ifzE e0 e1 e2)
               (if (= (eval-body e0 env) 0)
                   (eval-body e1 env)
                   (eval-body e2 env))]
              [(letE x e1 e2)
               (let ([v1 (eval-body e1 env)])
                 (eval-body e2 (extend-env env x v1)))]
              [(appE x es)
               (let ([f (lookup-env-fun x global-env)])
                 (eval-body (snd f)
                            (apply (fst f)
                                   (map (lambda (x) (eval-body x env)) es)
                                   global-env)))]
              [else (error 'eval "Syntax error: Expression other than that available in the body detected")]))
          
          (define (initialize [es : (Listof Exp)] [env : Env]) : Env
            (type-case (Listof Exp) es
              [(cons e es)
               (type-case Exp e
                 [(funE x xs e) (initialize es (extend-env-fun env x xs e))]
                 [else (error 'eval "Syntax error: An expression other than define is detected")])]
              [else (begin
                      (set! global-env env)
                      env)]))]
    
    (type-case Exp e
      [(defineE ds e) (eval-body e (initialize ds env))]
      [else (error 'eval "Syntax error: An expression other than define is detected")])))


(define (run [s : S-Exp]) : Value
  (eval (parse s) mt-env))