(define (puts x)
  (display x)
  (newline))

(define (last-pair l)
  (if (or (null? l) (null? (cdr l)))
    l
    (last-pair (cdr l))))

(puts (last-pair (list 23 72 149 34)))
(puts (last-pair (list 5)))
(puts (last-pair (list)))
