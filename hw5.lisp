(defun TERMINAL_TEST (state curr_move symbol)
    (setq row (floor (/ curr_move 4)))
    (setq col (mod curr_move 4))

    (if (or
        ;;; check right
        (if (< col 2)
            (if (and
                (equal symbol (aref state row (+ col 1)))
                (equal symbol (aref state row (+ col 2)))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))         
        )
        ;;; check left
        (if (> col 1)
            (if (and
                (equal symbol (aref state row (- col 1)))
                (equal symbol (aref state row (- col 2)))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))         
        )
        ;;; check up
        (if (> row 1)
            (if (and
                (equal symbol (aref state (- row 1) col))
                (equal symbol (aref state (- row 2) col))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t)) 
        )
        ;;; check down
        (if (< row 2)
            (if (and
                (equal symbol (aref state (+ row 1) col))
                (equal symbol (aref state (+ row 2) col))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))
        )
        ;;; check up left
        (if (and (> col 1)(> row 1))
            (if (and
                (equal symbol (aref state (- row 1) (- col 1)))
                (equal symbol (aref state (- row 2) (- col 2)))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))
        )
        ;;; check up right
        (if (and (< col 2)(> row 1))
            (if (and
                (equal symbol (aref state (- row 1) (+ col 1)))
                (equal symbol (aref state (- row 2) (+ col 2)))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))
        )
        ;;; check down left
        (if (and (> col 1)(< row 2))
            (if (and
                (equal symbol (aref state (+ row 1) (- col 1)))
                (equal symbol (aref state (+ row 2) (- col 2)))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))
        )
        ;;; check down right
        (if (and (< col 2)(< row 2))
            (if (and
                (equal symbol (aref state (+ row 1) (+ col 1)))
                (equal symbol (aref state (+ row 2) (+ col 2)))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))
        )
        ;;; check neighbros
        (if (and (or (equal col 1)(equal col 2) ) (or (equal row 1)(equal row 2) ) )
            (if (or
                ;;; left top , bottom right
                (and
                    (equal symbol (aref state (- col 1) (- row 1)))
                    (equal symbol (aref state (+ col 1) (+ row 1)))
                    (not (equal symbol '*))
                ) 
                ;;; right top, bottom left
                (and
                    (equal symbol (aref state (+ col 1) (- row 1)))
                    (equal symbol (aref state (- col 1) (+ row 1)))
                    (not (equal symbol '*))
                )
            )(return-from TERMINAL_TEST t))
        )
        ;;; left to right
        (if (or (= col 1)(= col 2))
            (if (and
                (equal symbol (aref state row (- col 1)))
                (equal symbol (aref state row (+ col 1)))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))
        )
        ;;; up and down
        (if (or (= row 1)(= row 2))
            (if (and
                (equal symbol (aref state (- row 1) col))
                (equal symbol (aref state (+ row 1) col))
                (not (equal symbol '*))
            ) (return-from TERMINAL_TEST t))
        )

    ) (return-from TERMINAL_TEST t))
    
    (return-from TERMINAL_TEST nil)
)

;;; defun TERMINAL_TEST (state curr_move symbol)

(defun RESULT (state move player)
    (setq row (floor (/ move 4)))
    (setq col (mod move 4))
    (setf (aref state row col) player)
)

(defun ACTIONS (state)
    (setq moves (list '()))
    (dotimes (i 4)
        (dotimes(j 4)
            (if (equal (aref state i j) '*) (
                setq moves (append moves (list (+ (* i 4) j)))
            ))
        )

    )
    
    (return-from ACTIONS (cdr moves))
)

(defun UTILITY (state curr_move symbol)
    (if (equal (TERMINAL_TEST state curr_move symbol) 't)
        do(
            if (equal symbol 'x) (return-from UTILITY 1)
            if (equal symbol 'o) (return-from UTILITY -1)
        )
    )

    (return-from UTILITY 0)
) 

(defun minimax (board)
    (setq actions (ACTIONS board))
    (setq best_action (car actions))
    (print "here 3")
    (loop for a in actions do
        ;;; replicate board
        (setq new_state (make-array '(4 4):displaced-to board))
        (RESULT new_state a 'o)
        (print "here 4")
        (setq a_score (Min_Value new_state a))
        (print "here 6")
        (if (> a_score best_score)(setq best_score a_score))
        (if (> a_score best_score)(setq best_action a))
    )

    (return-from minimax best_action)
)

(defun Max_Value (state curr_move)
    ;;; if TERMINAL_TEST return UTILITY ;;;
    (setq v -10000)
    (loop for a in (ACTIONS state) do
        ;;; replicate board
        (setq new_state (make-array '(4 4):displaced-to state))
        (Result(new_state a 'x)
        (print "here 5")
        (setq a_score (Min_Value new_state a)))
        (if (> temp v)(steq v temp))
    )
    (return-from Max_Value v)
)

(defun Min_Value (state curr_move)
    ;;; if TERMINAL_TEST return UTILITY ;;;
    (print "here 6")
    (setq v 10000)
    (loop for a in (ACTIONS state) do
        ;;; replicate board
        (print "here 7")
        (setq new_state (make-array '(4 4):displaced-to state))
        (print "here 8")
        (RESULT new_state a 'o)
        (setq temp (Max_Value new_state a))
        (if  (> v temp) (setq v temp))
    )
    
    (return-from Min_Value v)
)

(setq board (make-array '(4 4):initial-contents
    '(
        (* * * *)
        (* * * *)
        (* * * *)
        (* * * *)
    ))
)


(setq continue_playing 't)
(loop while (equal continue_playing 't) do
    (format t "~%~%")
    (princ "Enter your move : ")
    (setq x_move (read))
    (format t "~%~%")
    (RESULT board x_move 'x)
    (print board)
    (format t "~%~%")

    (print "here")
    (if (equal (TERMINAL_TEST board x_move 'x) 't) (print "Player X won"))
    (if (equal (TERMINAL_TEST board x_move 'x) 't) (setq continue_playing nil))
    (if (equal (TERMINAL_TEST board x_move 'x) 't) (format t "~%~%"))
    (print "here 1")
    ;;; construct new board
    (setq temp_board (make-array '(4 4):displaced-to board))
    (print temp_board)
     (print "here 2")
    ;;; calculate AI move
    (setq ai_move (minimax temp_board))
)
