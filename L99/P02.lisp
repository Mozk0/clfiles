(DEFUN MY-BUT-LAST (LIST)
  (LOOP :FOR SUBLIST :ON LIST
     :IF (NULL (CDDR SUBLIST))
     :RETURN SUBLIST))