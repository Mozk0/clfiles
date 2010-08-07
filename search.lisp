(DEFMACRO NAMED-LET (NAME BINDINGS &BODY BODY)
  (LET ((RECUR (GENSYM "recur"))
        (|return| (GENSYM "return"))
        (VARS (MAPCAR #'CAR BINDINGS)))
    `(MACROLET ((,NAME ,VARS
                  `(PROGN (PSETF ,,@(MAPCAN #'(LAMBDA (X)
                                                `(',X ,X))
                                            VARS))
                          (GO ,',RECUR))))
       (LET ,BINDINGS
         (BLOCK ,|return|
           (TAGBODY
              ,RECUR
              (RETURN-FROM ,|return|
                ,@BODY)))))))

(DEFVAR *TESTS* NIL)

(DEFSTRUCT TEST NAME FUNC)

(DEFMACRO DEFTEST (TEST-NAME &BODY EXPR)
  `(PUSHNEW (MAKE-TEST :NAME ',TEST-NAME
                       :FUNC #'(LAMBDA ()
                                 ,@EXPR))
            *TESTS*
            :KEY #'TEST-NAME
            :TEST #'EQ))

(DEFTEST NAMED-LET
  (= (NAMED-LET FACT ((N 10) (ACC 1))
       (IF (= N 0)
           ACC
           (FACT (1- N) (* ACC N))))
     3628800))

(DEFTEST MATH
  (= 2 (+ 1 3)))

(DEFUN RUN-TESTS ()
  (LABELS ((RUN-TEST (TEST)
             (LET ((TEST-NAME (TEST-NAME TEST))
                   (TEST-FUNC (TEST-FUNC TEST)))
               (FORMAT T "~a:" TEST-NAME)
               (IF (FUNCALL TEST-FUNC)
                   (PROGN
                     (FORMAT T "Pass~%")
                     T)
                   (PROGN
                     (FORMAT T "**Fail**~%")
                     NIL)))))
    (LET ((FAIL-COUNT (COUNT NIL (MAPCAR #'RUN-TEST
                                         *TESTS*))))
      (IF (= 0 FAIL-COUNT)
          (FORMAT T "Tests passed.~%")
          (FORMAT T "~a test~a failed.~%" FAIL-COUNT
                                          (IF (= FAIL-COUNT 1)
                                              ""
                                              "s"))))))

(DEFMACRO BIND (BINDINGS &BODY BODY)
  `(LOCALLY (DECLARE (SPECIAL ,@BINDINGS))
     ,@BODY))

(DEFUN DFS (GRAPH ROOT &OPTIONAL (GET-ADJ-FN #'ASSOC))
  "(DFS '((:A :B :C :E) (:B :C) (:C :D) (:D) (:E))
        :A
        #'ASSOC)
   => (:A :B :C :D :E)"
  (LABELS ((VISIT (NODE)
             (BIND (ACC HISTORY)
               (SETF (GETHASH NODE HISTORY) T)
               (PUSH NODE ACC)))
           (MAKE-EMPTY-STACK ()
             NIL)
           (MAKE-EMPTY-HISTORY ()
             (MAKE-HASH-TABLE))
           (STACK-PUSH (NODE)
             (BIND (STACK)
               (PUSH NODE STACK)))
           (STACK-POP ()
             (BIND (STACK)
               (POP STACK)))
           (VISITED-P (NODE)
             (BIND (HISTORY)
               (GETHASH NODE HISTORY)))
           (STACK-EMPTY-P ()
             (BIND (STACK)
               (NULL STACK))))
    (DO ((STACK (MAKE-EMPTY-STACK) (REMOVE-IF #'VISITED-P STACK))
         (HISTORY (MAKE-EMPTY-HISTORY) HISTORY)
         (ACC NIL ACC)
         (NODE ROOT (STACK-POP)))
        ((AND (STACK-EMPTY-P)
              (NOT (EQL ROOT NODE)))
         (REVERSE ACC))
      (DECLARE (SPECIAL ACC HISTORY STACK))
      (UNLESS (VISITED-P NODE)
        (VISIT NODE)
        (MAPC #'STACK-PUSH (REVERSE (FUNCALL GET-ADJ-FN NODE GRAPH)))))))