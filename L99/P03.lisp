(DEFUN ELEMENT-AT (LIST INDEX)
  (LOOP :FOR ELEMENT :IN LIST
        :AND I :FROM 1
     :IF (= I INDEX)
     :RETURN ELEMENT))