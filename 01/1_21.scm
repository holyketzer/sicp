(define (square x) (* x x))

(define (smallest-divisor n)
  (smallest-divisor-iter 2 n))

(define (remainder a n)
  (- a (* n (truncate (/ a n)))))

(define (smallest-divisor-iter x n)
  (cond ((> (square x) n) n)
        ((= (remainder n x) 0) x)
        (else (smallest-divisor-iter (+ x 1) n))))

(display (smallest-divisor 199))
(newline)
(display (smallest-divisor 1999))
(newline)
(display (smallest-divisor 19999))
(newline)