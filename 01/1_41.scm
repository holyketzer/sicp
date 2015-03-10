(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(display (inc 2))
(newline)
(display ((double inc) 2))
(newline)
(display (((double (double double)) inc) 5))
(newline)