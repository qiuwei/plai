#lang plai-typed

(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")])
  )

(define-type Binding
  [binding (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define (string-join [listS : (listof string)]) : string
  (foldr string-append "" listS))

(define (interp [expr : ExprC] [env : Env]) : Value
  (begin
    (display (string-join (list "Interpreting " (to-string expr) "\nwith Env " (to-string Env) "\n"))) 
    (type-case ExprC expr
      [numC (n) (numV n)]
      [idC (n) (lookup n env)]
      [appC (f a) (local ([define fun-value (interp f env)])
                    (interp (closV-body fun-value) (extend-env (binding (closV-arg fun-value) (interp a env)) (closV-env fun-value))))]
      [plusC (l r) (num+ (interp l env) (interp r env))]
      [multC (l r) (num* (interp l env) (interp r env))]
      [lamC (a b) (closV a b env)]
      )
    )
  )

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))

(define (lookup [n : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "couldn't resolve symbol")]
    [(symbol=? n (binding-name (first env))) (binding-val (first env))]
    [else (lookup n (rest env))]))



(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))
