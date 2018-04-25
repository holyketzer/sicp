(define (puts x)
  (display x)
  (newline))

(define (pascal row col)
  (cond
    ((= row 1) 1)
    ((= col 1) 1)
    ((= row col) 1)
    (else (+
      (pascal (- row 1) (- col 1))
      (pascal (- row 1) col)))))

(puts (pascal 2 2))
(puts (pascal 3 2))
(puts (pascal 4 3))
(puts (pascal 5 2))
(puts (pascal 5 3))
