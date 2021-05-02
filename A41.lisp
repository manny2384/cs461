(setq a (list 'w 'x 'y 'z))
(setq b (list '1 '2 '3 '4))

(print (reverse a))

(print 
    (append 
        (list (car (cdr b))) 
        (last b) 
        (list (car (cdr (reverse a))))
        (list (append (list (car (cdr a))) (last b)))
    )
)

(print (cons a (car (cdr b))))

(print
    (cons 
        (append
            (list 
                (append
                    (list (car a))
                    (list (car (cdr (reverse a))))
                )
            )
            (list 
                (append
                    (list (car (cdr b)))
                    (last b)   
                )
            )
        )
        (car a)
    )
)

(print 
    (append
        (list (append 
            (list (car a))
            (list (* (car b) 2))
        ))
        (list (append 
            (list (car (cdr a)))
            (list (* (car (cdr b)) 2))
        ))
        (list (append 
            (list (car (cdr (cdr a))))
            (list (* (car (cdr (cdr b))) 2))
        ))
        (list (append 
            (list (car (cdr (cdr (cdr a)))))
            (list (* (car (cdr (cdr (cdr b)))) 2))
        ))
    )
)
