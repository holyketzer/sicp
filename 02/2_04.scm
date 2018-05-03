(define (puts x)
  (display x)
  (newline))

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(puts (car (cons 3 10)))
(puts (cdr (cons 3 10)))
