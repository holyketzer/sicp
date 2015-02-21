(define (cont-frac-rec n d k)
  (define (iter i)
    (if (= i 0)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (iter (- i 1))))))

  (iter k))

(define (cont-frac-iter n d k)
  (define (iter i res)
    (if (= i 0)
      res
      (iter (- i 1) (/ (n i) (+ (d i) res)))))

  (iter k 0))

(display (/ 1 1.61803398875))
(newline)

(display (cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 10))
(newline)

(display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11))
(newline)