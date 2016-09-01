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
  [binding (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define fdC_double (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
(fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd) (extend-env (binding (fdC-arg fd) (interp a env fds)) mt-env)
                          fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
  )
)

(define (lookup [n : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "couldn't resolve symbol")]
    [(symbol=? n (binding-name (first env))) (binding-val (first env))]
    [else (lookup n (rest env))]))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))


(test (interp (appC 'quadruple (plusC (numC 3) (numC 2))) mt-env (list fdC_double (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))) 20)

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)
 
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
 
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)


//bug appears
(test (interp  (appC 'double (plusC (numC 1) (numC 2)))
              mt-env
              (list (fdC 'double 'x (appC 'threetimes (plusC (idC 'x) (idC 'x))))
                    (fdC 'threetimes 'y (multC (numC 3) (idC 'x)))))
      9)
