(DEFPACKAGE :COMMON-LISP-USER-MACRO
  (:NICKNAMES :CL-USER-MACRO :MACRO )
  (:USE :COMMON-LISP)
  (:EXPORT :TAIL-RECUR
           :LOCALLY-OPTIMIZE
           :DEFUN/TYPE
           :LET/TYPE
           :DO/TYPE
           :WITH-GENSYM
           :DOFOR
           :WHILE-LOOP
           :FOR-LOOP
           :DEFSEQ
           :WITH-IN-PACKAGE))

(IN-PACKAGE :COMMON-LISP-USER-MACRO)

(DEFMACRO TAIL-RECUR (NAME BINDINGS &BODY BODY)
  "transform recursion in tail position into loop.
   Usage:
  (TAIL-RECUR RECUR ((I N) (ACC 1))
    (IF (= I 0)
        ACC
        (RECUR (1- I) (* ACC I))))"
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
           (RETURN-FROM ,|return|
             (TAGBODY
                ,RECUR
                ,@BODY)))))))

(DEFMACRO LOCALLY-OPTIMIZE (OPTIMIZE-OPTIONS &BODY BODY)
  "locally declare optimization.
   Usage:
  (LOCALLY-OPTIMIZE ((SPEED 3) (SAFETY 0))
    (HEAVY-CALCULATION ARG0 ARG1))"
  `(LOCALLY (DECLARE (OPTIMIZE ,@OPTIMIZE-OPTIONS))
     ,@BODY))

(DEFMACRO DEFUN/TYPE (FUN-NAME ARG RETURN-TYPE ARG-TYPE &BODY BODY)
  "define function with ftype declaration.
   Usage:
   (DEFUN/TYPE
        FIXNUM (FIXNUM FIXNUM)
        ADD    (A      B)
      (+ A B))"
  `(PROGN
     (DECLAIM (FTYPE (FUNCTION ,ARG-TYPE ,RETURN-TYPE) ,FUN-NAME))
     (DEFUN ,FUN-NAME ,ARG
       ,@BODY)))

(DEFMACRO DO/TYPE (BINDINGS CONDITION &BODY BODY)
  "DO with type declaration.
   Usage:
  (DO/TYPE ((FIXNUM I    N (1- I))
            (FIXNUM ACC  1 (* N ACC)))
      ((= I 0) ACC))"
  (LABELS ((GET-DECLARATION (BINDING)
             (DESTRUCTURING-BIND (TYPE VAR . _) BINDING
               (DECLARE (IGNORE _))
               `(DECLARE (TYPE ,TYPE ,VAR))))
           (GET-BINDING-WITHOUT-TYPE (BINDING)
             (DESTRUCTURING-BIND (_ . BINDING-WITHOUT-TYPE) BINDING
               (DECLARE (IGNORE _))
               BINDING-WITHOUT-TYPE)))
    (LET ((DECLARATIONS (MAPCAR #'GET-DECLARATION
                                BINDINGS))
          (BINDINGS-WITHOUT-TYPE  (MAPCAR #'GET-BINDING-WITHOUT-TYPE
                                          BINDINGS)))
      `(DO ,BINDINGS-WITHOUT-TYPE
           ,CONDITION
           ,@DECLARATIONS
         ,@BODY))))

(DEFMACRO LET/TYPE (BINDINGS &BODY BODY)
  "LET with type declaration.
   Usage:
  (LET/TYPE ((FIXNUM I 2))
    (+ I 3))"
  (LABELS ((GET-DECLARATION (BINDING)
             (DESTRUCTURING-BIND (TYPE VAR . _) BINDING
               (DECLARE (IGNORE _))
               `(DECLARE (TYPE ,TYPE ,VAR))))
           (GET-BINDING-WITHOUT-TYPE (BINDING)
             (DESTRUCTURING-BIND (_ . BINDING-WITHOUT-TYPE) BINDING
               (DECLARE (IGNORE _))
               BINDING-WITHOUT-TYPE)))
    (LET ((DECLARATIONS (MAPCAR #'GET-DECLARATION
                                BINDINGS))
          (BINDINGS-WITHOUT-TYPE  (MAPCAR #'GET-BINDING-WITHOUT-TYPE
                                          BINDINGS)))
      `(LET ,BINDINGS-WITHOUT-TYPE
            ,@DECLARATIONS
         ,@BODY))))

(DEFMACRO WITH-GENSYM (VARS &BODY BODY)
  "Bind vars to gensymed symbols.
   Usage:
  (WITH-GENSYM (TMP)
    `(PROGN
       (SETF ,TMP ,X)
       (SETF ,X   ,Y)
       (SETF ,Y ,TMP)))"
  (LET ((BINDS (MAPCAR #'(LAMBDA (VAR)
                           `(,VAR (GENSYM ,(FORMAT NIL "~a" VAR))))
                       VARS)))
  `(LET ,BINDS
     ,@BODY)))

(DEFMACRO WHILE-LOOP (CONDITION &BODY BODY)
  (WITH-GENSYM (TAG)
    `(TAGBODY
        ,TAG
        ,@BODY
        (WHEN ,CONDITION
          (GO ,TAG)))))

(DEFMACRO FOR-LOOP (INITIAL CONDITION POST &BODY BODY)
  (WITH-GENSYM (TAG)
    `(TAGBODY
        ,INITIAL
        ,TAG
        (WHEN ,CONDITION
          ,@BODY
          ,POST
          (GO ,TAG)))))

(DEFMACRO DOFOR ((VAR FROM TO) &BODY BODY)
  `(DO ((,VAR ,FROM (1+ ,VAR)))
       ((= ,VAR ,TO))
     ,@BODY))
