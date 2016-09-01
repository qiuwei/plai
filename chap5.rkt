Welcome to DrRacket, version 6.6 [3m].
Language: plai-typed, with debugging; memory limit: 128 MB.
(fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
(good (interp (appC 'quadruple (plusC (numC 3) (numC 2))) mt-env (list fdC_double (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))) 20 20 "at line 62")
(good (interp (plusC (numC 10) (appC 'const5 (numC 10))) mt-env (list (fdC 'const5 '_ (numC 5)))) 15 15 "at line 64")
(good (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2)))) mt-env (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 16 16 "at line 69")
(good (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2)))) mt-env (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))) (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 22 22 "at line 74")
(exception (interp (appC 'double (plusC (numC 1) (numC 2))) mt-env (list (fdC 'double 'x (appC 'threetimes (plusC (idC 'x) (idC 'x)))) (fdC 'threetimes 'y (multC (numC 3) (idC 'x))))) "lookup: couldn't resolve symbol" 9 "at line 82")
> 