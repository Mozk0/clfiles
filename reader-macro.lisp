(DEFPACKAGE :READER-MACRO
  (:USE :COMMON-LISP)
  (:EXPORT :SYMB :SET-DEBUG :UNSET-DEBUG))

(IN-PACKAGE :READER-MACRO)

(DEFUN SYMB (&REST ARGS)
  (INTERN (FORMAT NIL "~{~A~}" ARGS)))

(DEFUN FLATTEN (LIST)
  (IF LIST
      (MAPCAN #'(LAMBDA (X)
                  (IF (LISTP X)
                      (FLATTEN X)
                      `(,X)))
              LIST)
      NIL))

;;;; '#_ Package Form' reads the Form in the Package.
(DEFUN |#_-reader| (STREAM CHAR N)
  (DECLARE (IGNORE CHAR N))
  (LET ((*PACKAGE* (FIND-PACKAGE (READ STREAM T NIL T))))
    (READ STREAM T NIL T)))

(SET-DISPATCH-MACRO-CHARACTER #\# #\_ #'|#_-reader|)

;;;; '#F(+ 1 %1 %2) ;=> (lambda (%1 %2) (+ 1 %1 %2))
(DEFUN INTEGER-STRING-P (STRING)
  (= (NTH-VALUE 1 (PARSE-INTEGER STRING :JUNK-ALLOWED T))
     (LENGTH STRING)))

(DEFUN GET-ARITY (BODY)
  (LOOP :FOR E :IN (FLATTEN BODY)
        :WITH ARITY-SPECIFIED-P := NIL 
     :IF (AND (SYMBOLP E)
              (CHAR= (CHAR (SYMBOL-NAME E) 0)
                     #\%)
              (INTEGER-STRING-P (SUBSEQ (SYMBOL-NAME E) 1)))
        :MAXIMIZE (OR (PARSE-INTEGER (SUBSEQ (SYMBOL-NAME E) 1) :JUNK-ALLOWED T)
                      1)
           :INTO ARITY
        :DO (SETF ARITY-SPECIFIED-P T)
     :FINALLY
        (UNLESS (AND ARITY-SPECIFIED-P (TYPEP ARITY '(INTEGER 0)))
          (ERROR "The arity of #F form couldn't be specified."))
        (RETURN
          (IF ARITY-SPECIFIED-P
              ARITY
              0))))

(DEFUN |#F-reader| (STREAM CHAR N)
  (DECLARE (IGNORE CHAR))
  (LET* ((BODY (READ STREAM T NIL T))
         (ARITY (OR N
                    (IF (LISTP BODY)
                        (GET-ARITY BODY)
                        (GET-ARITY `(,BODY))))))
    (IF (= ARITY 1)
        `(LAMBDA (,(INTERN "%1"))
           (SYMBOL-MACROLET ((,(INTERN "%") ,(INTERN "%1")))
             ,BODY))
        `(LAMBDA ,(LOOP :FOR I :FROM 1 :TO ARITY
                     :COLLECT (SYMB '% I))
           ,BODY))))

(SET-DISPATCH-MACRO-CHARACTER #\# #\F #'|#F-reader|)

;;;; print debug macro
(DEFVAR *DEBUG* NIL)

(DEFMACRO SET-DEBUG ()
  `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF *DEBUG* T)))

(DEFMACRO UNSET-DEBUG ()
  `(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
     (SETF *DEBUG* NIL)))    

(DEFUN |#>-reader| (STREAM CHAR N)
  (DECLARE (IGNORE CHAR))
  (LET ((BODY (READ STREAM T NIL T)))
    (IF *DEBUG*
        (LET ((RES '#:RES))
          `(LET ((,RES ,BODY))
             (FRESH-LINE)
             (FORMAT T "~a => " ,N)
             (PRIN1 ,RES)))
        BODY)))

(SET-DISPATCH-MACRO-CHARACTER #\# #\> #'|#>-reader|)

;;;; aref macro
(DEFUN |$-reader| (STREAM CHAR)
    (DECLARE (IGNORE CHAR))
    (LET* ((ACCESS (READ-DELIMITED-LIST #\] STREAM T))
           (ACCESS-STRING (PRIN1-TO-STRING ACCESS))
           (POSITION-[ (POSITION #\[ ACCESS-STRING))
           (ARRAY-NAME (SUBSEQ ACCESS-STRING 1 POSITION-[))
           (ARRAY-INDEX-STRING (CONCATENATE 'STRING
                                 "("
                                 (SUBSEQ ACCESS-STRING
                                         (1+ POSITION-[)
                                         (1- (LENGTH ACCESS-STRING)))
                                 ")"))
           (ARRAY-INDEX (READ-FROM-STRING ARRAY-INDEX-STRING)))
      ARRAY-INDEX
      `(AREF ,(INTERN ARRAY-NAME) ,@ARRAY-INDEX)))

(SET-MACRO-CHARACTER #\] (GET-MACRO-CHARACTER #\)))
(SET-MACRO-CHARACTER #\$ #'|$-reader|)
             
