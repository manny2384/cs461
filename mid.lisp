(defun my_function (list_1 list_2) 
    (return-from my_function (append (list list_1) (list list_2)))
)

(print
    (my_function (list 'a 'b 'c) (list '1 '2 '3))
)

(defun circle_area (radius)
    (setq pi 3.14159)
    (return-from circle_area (* (* radius radius) pi))
)

(print
    (circle_area 4)
)