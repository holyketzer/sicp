(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i r)
    (if (= i 1)
      r
      (iter (- i 1) (compose f r))))
  (iter n f))

(display ((repeated square 2) 5))
(newline)