(define (puts x)
  (display x)
  (newline))

(define (fringe l)
  (if (list? l)
    (if (null? l)
      l
      (append (fringe (car l)) (fringe (cdr l))))
    (list l)))

(define x (list (list 1 2) (list 3 4)))

(puts (fringe x))
(puts (fringe (list x x)))
