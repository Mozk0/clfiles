(DEFPACKAGE :LAZY-SEQUENCE
  (:NICKNAMES :LAZYSEQ :LS :LAZY)
  (:USE :COMMON-LISP)
  (:SHADOW :MAPCAR :REDUCE :CAR :CDR :SUBSEQ
           :REMOVE-IF-NOT :NTH :MAP)
  (:EXPORT :LAZY :MAPCAR :FILTER :FORCE :TAKE :MAP
           :DROP :ITERATE :CAR :CDR :SUBSEQ :REDUCE
           :NTH :REPEAT :INTERLEAVE :NATURALS :INTEGERS
           :DEFSEQ :DEFLAZY :PRIMES))

(IN-PACKAGE :LAZY-SEQUENCE)

(DEFSTRUCT THUNK
  (FN #'(LAMBDA () NIL) :TYPE FUNCTION)
  (CACHE NIL))

(DEFMACRO LAZY (&BODY BODY)
  "Stop evaluation until force is called."
  `(MAKE-THUNK :FN #'(LAMBDA ()
                       ,@BODY)))

(DEFUN FORCE (X)
  "Evaluate lazy objects."
  (COND ((AND (THUNK-P X) (THUNK-CACHE X)) (THUNK-CACHE X))
        ((THUNK-P X) (SETF (THUNK-CACHE X) (FORCE (FUNCALL (THUNK-FN X)))))
        (T X)))

(DEFUN CAR (X)
  "If x is lazy cons, get the car."
  (CL:CAR (FORCE X)))

(DEFUN CDR (X)
  "If x is lazy cons, get the cdr."
  (CL:CDR (FORCE X)))

(DEFUN MAPCAR (FN &REST SEQS)
  "(MAPCAR FN '(A1 A2 A3 ...)) ;=> ((FN A1) (FN A2) (FN A3) ...)"
  (LAZY
   (CONS (APPLY FN (CL:MAPCAR #'CAR SEQS))
         (APPLY #'LAZYSEQ:MAPCAR FN (CL:MAPCAR #'CDR SEQS)))))

(SETF (SYMBOL-FUNCTION 'MAP) #'MAPCAR)

(DEFUN TAKE (N X)
  "Take first n elements of x."
  (LOOP :REPEAT N
        :FOR SEQ := X :THEN (CDR SEQ)
     :COLLECT (CAR SEQ)))

(DEFUN DROP (N X)
  "Drop first n elements of x."
  (LOOP :FOR SEQ := X :THEN (CDR SEQ)
        :REPEAT N
     :FINALLY (RETURN SEQ)))

(DEFUN %ITERATE (FN X)
  (LAZY
    (CONS X
          (%ITERATE FN (FUNCALL FN X)))))

(DEFUN ITERATE (FN &REST ARGS)
  "(ITERATE FN A) ;=> (A (FN A) (FN (FN A)) ...)"
  (MAPCAR #'CAR
          (%ITERATE #'(LAMBDA (X)
                        `(,@(CDR X) ,(APPLY FN X)))
                    ARGS)))

(DEFUN LAZY-REDUCE (FN LIST &OPTIONAL ACC)
  (LAZY
    (IF (FORCE LIST)
        (FUNCALL FN (CAR LIST) (LAZY-REDUCE FN (CDR LIST) ACC))
        ACC)))

(DEFUN REDUCE (&REST ARGS)
  (APPLY #'CL:REDUCE (MAPCAR #'FORCE ARGS)))

(SETF (SYMBOL-FUNCTION 'REMOVE-IF-NOT) #'REDUCE)

(DEFUN FILTER (FN LIST)
  (LAZY-REDUCE #'(LAMBDA (X REST)
                   (IF (FUNCALL FN X)
                       (CONS X REST)
                       REST))
               LIST
               NIL))

(DEFUN SUBSEQ (SEQ START END)
  (TAKE (- END START) (DROP START SEQ)))

(DEFUN NTH (N SEQ)
  (CAR (DROP N SEQ)))

(DEFUN REPEAT (&REST ARGS)
  "(REPEAT A B C) ;=> (A B C A B C ...)"
  (LAZY
    (APPEND ARGS
            (APPLY #'REPEAT ARGS))))

(DEFUN NATURALS ()
  "(0 1 2 3 ...)"
  (ITERATE #'1+ 0))

(DEFUN INTERLEAVE (&REST SEQS)
  "(INTERLEAVE (1 2 3 ...) (4 5 6 ...)) ;=> (1 4 2 5 3 6 ...)"
  (LAZY
    (APPEND (CL:MAPCAR #'CAR SEQS)
            (APPLY #'INTERLEAVE (CL:MAPCAR #'CDR SEQS)))))

(DEFUN INTEGERS ()
  "(0 1 -1 2 -2 3 -3...)"
  (INTERLEAVE (MAPCAR #'- (NATURALS))
              (CDR (NATURALS))))
    
(DEFUN FIBS ()
  "(0 1 1 2 3 5 8 13 21 34 55 ...)"
  (ITERATE #'+ 0 1))

(DEFMACRO DEFLAZY (NAME ARGS &BODY BODY)
  `(DEFUN ,NAME ,ARGS
     (LAZY
       ,@BODY)))

(DEFUN SIEVE (LIST)
  (LAZY
    (CONS (CAR LIST)
          (SIEVE (FILTER #'(LAMBDA (X)
                             (/= (REM X
                                      (CAR LIST))
                                 0))
                         (CDR LIST))))))

(DEFUN PRIMES ()
  (SIEVE (DROP 2
               (NATURALS))))
