(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
      res
      (iter (- i 1) (/ (n i) (+ (d i) res)))))

  (iter k 0))

(define (d i)
  (if (= (remainder i 3) 2)
    (+ (* (quotient i 3) 2) 2)
    1))

(display (- 2.71828182846 2))
(newline)

(display (cont-frac (lambda (i) 1.0) d 20))
(newline)