(define (puts x)
  (display x)
  (newline))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(puts (A 1 10))
(puts (A 2 4))
(puts (A 3 3))
