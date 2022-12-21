
(setq max_depth 3) ;;; helps determine whether to continue search or return

;;; @param state = current board
;;; @param symbol = current player
;;; @param first_row, first_col, sec_col, sec_row = help determine winning state
;;; returns 't if squares compared are identical symbol other than init symbols
(defun COMPARE_SQUARES (state symbol first_row sec_row first_col sec_col)
    (if (and
        (equal symbol (aref state first_row first_col))
        (equal symbol (aref state sec_row sec_col))
        (not (equal symbol '*))
    ) (return-from COMPARE_SQUARES t))
)

;;; @param state = current board
;;; @param curr_move = the current move being played
;;; @param symbol = current player making the move
;;; returns 't if win detected
(defun TERMINAL_TEST (state curr_move symbol)
    (setq row (floor (/ curr_move 4)))
    (setq col (mod curr_move 4))

    (if (or
        ;;; check right
        (if (< col 2)
            (if (equal 't (COMPARE_SQUARES state symbol row row (+ col 1) (+ col 2)))
                (return-from TERMINAL_TEST t))
        )
        ;;; check left
        (if (> col 1)
            (if (equal 't (COMPARE_SQUARES state symbol row row (- col 1) (- col 2)))
                (return-from TERMINAL_TEST t))   
        )
        ;;; check up
        (if (> row 1)
            (if (equal 't (COMPARE_SQUARES state symbol (- row 1) (- row 2) col col))
                (return-from TERMINAL_TEST t)) 
        )
        ;;; check down
        (if (< row 2)
            (if (equal 't (COMPARE_SQUARES state symbol (+ row 1) (+ row 2) col col))
                (return-from TERMINAL_TEST t)) 
        )
        ;;; check up left
        (if (and (> col 1)(> row 1))
            (if (equal 't (COMPARE_SQUARES state symbol (- row 1) (- row 2) (- col 1) (- col 2)))
                (return-from TERMINAL_TEST t)) 
        )
        ;;; check up right
        (if (and (< col 2)(> row 1))
            (if (equal 't (COMPARE_SQUARES state symbol (- row 1) (- row 2) (+ col 1) (+ col 2)))
                (return-from TERMINAL_TEST t)) 
        )
        ;;; check down left
        (if (and (> col 1)(< row 2))
            (if (equal 't (COMPARE_SQUARES state symbol (+ row 1) (+ row 2) (- col 1) (- col 2)))
                (return-from TERMINAL_TEST t)) 
        )
        ;;; check down right
        (if (and (< col 2)(< row 2))
            (if (equal 't (COMPARE_SQUARES state symbol (+ row 1) (+ row 2) (+ col 1) (+ col 2)))
                (return-from TERMINAL_TEST t)) 
        )
        ;;; check neighbros
        (if (and (or (equal col 1)(equal col 2) ) (or (equal row 1)(equal row 2) ) )
            (if (or
                ;;; left top , bottom right
                (equal 't (COMPARE_SQUARES state symbol (- row 1) (+ row 1) (- col 1) (+ col 1)))
                ;;; right top, bottom left
                (equal 't (COMPARE_SQUARES state symbol (- row 1) (+ row 1) (+ col 1) (- col 1)))
            )(return-from TERMINAL_TEST t))
        )
        ;;; left to right
        (if (or (= col 1)(= col 2))
            (if (equal 't (COMPARE_SQUARES state symbol row row (- col 1) (+ col 1)))
                (return-from TERMINAL_TEST t)) 
        )
        ;;; up and down
        (if (or (= row 1)(= row 2))
            (if (equal 't (COMPARE_SQUARES state symbol (- row 1) (+ row 1) col col))
                (return-from TERMINAL_TEST t)) 
        )
    ) (return-from TERMINAL_TEST t))
    
    (return-from TERMINAL_TEST nil)
)

;;; @param state = current board
;;; @param move = move made by player
;;; @param player = player making move
;;; returns state by reference
(defun RESULT (state move player)
    (setq row (floor (/ move 4)))
    (setq col (mod move 4))
    (setf (aref state row col) player)
)

;;; @param state = current board state
;;; returns a list of availble moves
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

;;; @param symbol = player's symbol
;;; returns 1 if x wins, -1 if o wins, 0 else
;;; Note: utility function came from an old javascript function I wrote for a connect 4 game
;;;         It was a tutorial on codecademy and so I've used the same function here
(defun UTILITY (symbol)
    (if (equal symbol 'x) (return-from UTILITY 1))
    (if (equal symbol 'o) (return-from UTILITY -1))
    (return-from UTILITY 0)
) 

;;; @param state = current board
;;; @param curr_move = move being tested
;;; @param depth = how far down the tree we are
;;; returns UTILITY when depth reached or TERMINAL_TEST is true
(defun Max_Value (state curr_move depth)

    ;;; if TERMINAL_TEST return UTILITY ;;;
    (RESULT state curr_move 'x)
    (if (equal (TERMINAL_TEST state curr_move 'o) 't)
        (progn
            (return-from Max_Value (UTILITY 'o))
        )
    )
    (if (equal (TERMINAL_TEST state curr_move 'x) 't)
        (progn
            (return-from Max_Value (UTILITY 'x))
        )
    )
    (if (or 
            (= (length (ACTIONS state)) 0) 
            (= depth 0)
        )
        (progn
            (return-from Max_Value 0)
        )
    )


    ;;; set of move and value
    (setq v -10000)

    (loop for a in (ACTIONS state) do
        ;;; replicate board
        (setq new_state (make-array '(4 4)))
        (COPY_ARRAY state new_state)

        (setq temp (Min_Value new_state a (- depth 1)) )
        (if (> temp v) (setq v temp))
    )

    (return-from Max_Value v)
)

;;; @param state = current board
;;; @param curr_move = move being tested
;;; @param depth = how far down the tree we are
;;; returns UTILITY when depth reached or TERMINAL_TEST is true
(defun Min_Value (state curr_move depth)
    ;;; if player x wins on next move, block that move
    (if (and
            (= depth max_depth)
            (or
                (equal (TERMINAL_TEST state curr_move 'x) 't)
                (equal (TERMINAL_TEST state curr_move 'o) 't)
            )
        )
        (return-from Min_Value -1000)
    )
    ;;; if TERMINAL_TEST return UTILITY ;;;
    (RESULT state curr_move 'o)
    (if (equal (TERMINAL_TEST state curr_move 'o) 't)
        (progn
            (return-from Min_Value (UTILITY 'o))
        )
    )
    (if (equal (TERMINAL_TEST state curr_move 'x) 't)
        (progn
            (return-from Min_Value (UTILITY 'x))
        )
    )
    (if (or 
            (= (length (ACTIONS state)) 0) 
            (= depth 0)
        )
        (progn
            (return-from Min_Value 0)
        )
    )

    (setq v 10000)

    (loop for a in (ACTIONS state) do
        ;;; replicate board
        (setq new_state (make-array '(4 4)))
        (COPY_ARRAY state new_state)
        
        (setq temp (Max_Value new_state a (- depth 1) ))

        (if  (> v temp) (setq v temp))
    )

    (return-from Min_Value v)
)


(defun minimax (board)
    (setq actions (ACTIONS board))
    (setq best_action (car actions))
    (setq new_state (make-array '(4 4)))
    (COPY_ARRAY board new_state)

    (RESULT new_state best_action 'o)

    (setq best_score 10000)

    (loop for a in actions do
        ;;; replicate board
        (setq new_state (make-array '(4 4)))
        (COPY_ARRAY board new_state)

        (setq a_score (Min_Value new_state a max_depth))

        (if (< a_score best_score)(progn
            (setq best_score a_score)
            (setq best_action a)
        ))
    )

    (return-from minimax best_action)
)

;;; init state of board
(setq init_board (make-array '(4 4):initial-contents
    '(
        (* * * *)
        (* * * *)
        (* * * *)
        (* * * *)
    ))
)

(defun COPY_ARRAY (array1 array2)
    (dotimes (i 4)
        (dotimes(j 4)
            (setf (aref array2 i j) (aref array1 i j))
        )

    )
)

(defun DISPLAY (state)
    (dotimes (i 4)
        (dotimes (j 4)
            (princ (aref state i j))
            (princ " ")
            (if (= (mod (+ j 1) 4) 0)
                (format t "~%")
            )
        )
    )
)

(print "Initial State of Board")
(format t "~%~%")
(DISPLAY init_board)

;;; ******** MAIN GAME LOGIC *********
;;; game continues until continue_playing is true
;;; when a terminal test is encountered, game ends
;;; when there are no moves available, game ends
(setq continue_playing 't)
(loop while (equal continue_playing 't) do
    ;;; if no moves availabe, tie no one WINS
    (if (= (length (ACTIONS init_board)) 0) (progn
        (print "TIE")
        (return nil)
    ))

    ;;; prompt user for move, loops prompt if player chooses unavailable move
    (princ "Enter your move [1 - 16]: ")
    (setq x_move (- (read) 1))
    (format t "~%")
    (loop while (not (member x_move (ACTIONS init_board)))
        do(progn
            (print "action not available, enter new move : ")
            (setq x_move (- (read) 1))
        )
    )

    ;;; perform player move
    (RESULT init_board x_move 'x)

    ;;; check if player x has won
    (if (equal (TERMINAL_TEST init_board x_move 'x) 't) (progn
        (print "YOU WIN!!!")
        (format t "~%~%")
        (return nil)
    ))

    ;;; construct new board to perform minimax
    (setq temp_board (make-array '(4 4)))
    (COPY_ARRAY init_board temp_board)

    ;;; calculate AI move
    (print "Calculating move ...")
    (setq ai_move (minimax temp_board))
    (princ "AI Move = ")
    (princ (+ ai_move 1))
    (format t "~%~%")

    ;;; perform ai move
    (RESULT init_board ai_move 'o)
    (if (equal (TERMINAL_TEST init_board ai_move 'o) 't) (progn
        (print "AI WINS!!!")
        (format t "~%~%")
        (return nil)
    ))

    ;;; display new state of game
    (DISPLAY init_board)
    (format t "~%~%")

)

;;; show board state once more
(DISPLAY init_board)
