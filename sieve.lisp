#_LS (DEFUN SIEVE (LIST)
       (LAZY
         (DESTRUCTURING-BIND (X . XS) (FORCE LIST)
           `(,X ,@(SIEVE (FILTER #F(/= (REM % X) 0)
                                 XS))))))

#_LS (DEFUN PRIMES ()
       (SIEVE (DROP 2 (NATURALS))))
