(DEFPARAMETER *TESTS* NIL)

(DEFMACRO EXAMPLE (EXPR _ EXPECT &KEY (TEST '#'EQUALP))
  (DECLARE (IGNORE _))
  (WITH-GENSYM (RESULT)
    `(LET ((,RESULT ,EXPR))
       (IF (FUNCALL ,TEST ,RESULT ',EXPECT)
           (FORMAT T "OK: ~a => ~a~%" ',EXPR ,RESULT)
           (FORMAT T "NG: ~a => ~a, expected ~a~%" ',EXPR ,RESULT ',EXPECT)))))

(DEFUN P01 (LIST)
  (LOOP :FOR SUBLIST :ON LIST
     :IF (NULL (CDR SUBLIST))
     :RETURN SUBLIST))
(EXAMPLE (P01 '(A B C D))
         => (D))

(DEFUN P02 (LIST)
  (LOOP :FOR SUBLIST :ON LIST
     :IF (NULL (CDDR SUBLIST))
     :RETURN SUBLIST))
(EXAMPLE (P02 '(A B C D))
         => (C D))

(DEFUN P03 (LIST INDEX)
  (LOOP :FOR ELEMENT :IN LIST
        :AND I :FROM 1
     :IF (= I INDEX)
     :RETURN ELEMENT))
(EXAMPLE (P03 '(A B C D E) 3)
         => C)

(DEFUN P04 (LIST)
  (LOOP :FOR _ :IN LIST
        :AND I :FROM 1
     :FINALLY (RETURN I)))
(EXAMPLE (P04 '(A B C D E))
         => 5)

(DEFUN P05 (LIST)
  (LOOP :FOR ELEM :IN LIST
        :WITH ACC := NIL
     :DO (PUSH ELEM ACC)
     :FINALLY (RETURN ACC)))
(EXAMPLE (P05 '(A B C D E))
         => (E D C B A))

(DEFUN P06 (LIST)
  (LOOP :FOR ELEM :IN LIST
        :WITH REVERSE := NIL
     :DO (PUSH ELEM REVERSE)
     :FINALLY (RETURN
                (LOOP :FOR E0 :IN LIST
                      :AND E1 :IN REVERSE
                   :ALWAYS (EQ E0 E1)))))
(EXAMPLE (P06 '(A B C B A))
         => T)
(EXAMPLE (P06 '(A B C D E))
         => NIL)

(DEFUN P07 (LIST)
  (LOOP :WITH ACC := NIL
        :WITH STACK := `(,LIST)
        :WITH LS
     :IF (NULL STACK)
       :RETURN (P05 ACC) ;REVERSE
     :ELSE
       :DO
         (SETF LS (POP STACK))
         (LOOP :FOR SUBLS :ON LS
               :FOR ELEM := (CAR SUBLS)
            :WHILE (ATOM ELEM)
              :DO (PUSH ELEM ACC)
            :FINALLY
              (WHEN SUBLS
                (PUSH (CDR SUBLS) STACK)
                (PUSH (CAR SUBLS) STACK)))))
(EXAMPLE (P07 '((1 2 (3 4)) (5 (6 7 (8 9 (10) (11 (12)))) 13 (14) 15)))
         => (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(EXAMPLE (P07 '(1 () 3))
         => (1 NIL 3))