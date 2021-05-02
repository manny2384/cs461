(defun TERMINAL_TEST (state_list)
    
    (if (or 
            (equal (car state_list) 'B) 
            (equal (car (cdr state_list)) 'B) 
            (equal (car (reverse state_list)) 'B)
        )

        (return-from TERMINAL_TEST nil)
    )
    (return-from TERMINAL_TEST t)
)
;;; Test TERMINAL_TEST ;;;
(princ "Testing TERMINAL_TEST with ('A 'A 'A), ('C 'C 'C), and ('A 'B 'C) respectively")
(print (TERMINAL_TEST (list 'A 'A 'A)))
(print (TERMINAL_TEST (list 'C 'C 'C)))
(print (TERMINAL_TEST (list 'A 'B 'C)))
(format t "~%~%")

(defun UTILITY (state_list)
    (if (and
            (equal (car state_list) 'A) 
            (equal (car (cdr state_list)) 'A) 
            (equal (car (reverse state_list)) 'A)
        )
        (return-from UTILITY 1)
    )

    (if (and
            (equal (car state_list) 'C) 
            (equal (car (cdr state_list)) 'C) 
            (equal (car (reverse state_list)) 'C)
        )
        (return-from UTILITY -1)
    )

    (return-from UTILITY 0)
)
;;; test UTILITY function ;;;
(princ "Testing UTILITY with ('A 'A 'A), ('C 'C 'C), and ('A 'B 'C) respectively")
(print (UTILITY (list 'A 'A 'A)))
(print (UTILITY (list 'C 'C 'C)))
(print (UTILITY (list 'A 'B 'C)))
(format t "~%~%")

(defun ACTIONS (state_list)
    ;;; initialize list of actions ;;;
    (setq my_actions (list '()))

    (if (equal (car state_list) 'B)
        (setq my_actions (append my_actions (list 'A1)))
    )
    (if (equal (car (cdr state_list)) 'B)
        (setq my_actions (append my_actions (list 'A2)))
    )
    (if (equal (car (reverse state_list)) 'B)
        (setq my_actions (append my_actions (list 'A3)))
    )

    ;;; remove nil from list of actions ;;;
    (setq my_actions (cdr my_actions))
    (return-from ACTIONS my_actions)
)
;;; Test ACTIONS ;;;
(princ "Testing ACTIONS with ('A 'A 'A), ('C 'C 'C), ('B 'B 'B) and ('A 'B 'C) respectively")
(print (ACTIONS (list 'A 'A 'A)))
(print (ACTIONS (list 'C 'C 'C)))
(print (ACTIONS (list 'B 'B 'B)))
(print (ACTIONS (list 'A 'B 'C)))
(format t "~%~%")

(defun RESULT (state_list action)
    (if (and (equal (car state_list) 'B) (equal action 'A1))
        (return-from RESULT 
            (append (list 'A) (cdr state_list))
        )
    )

    (if (and (equal (car (cdr state_list)) 'B) (equal action 'A2))
        (return-from RESULT 
            (append (list (car state_list)) (list 'A) (last state_list))
        )
    )

    (if (and (equal (car state_list) 'B) (equal action 'A3))
        (return-from RESULT 
            (append (list (car state_list)) (list (car (cdr state_list))) (list 'A))
        )
    )
)

;;; test RESULT function ;;;
(princ "Testing RESULT with ('B 'B 'B) with actions: A1, A2, and A3 respectively")
(print (RESULT (list 'B 'B 'B) 'A1))
(print (RESULT (list 'B 'B 'B) 'A2))
(print (RESULT (list 'B 'B 'B) 'A3))