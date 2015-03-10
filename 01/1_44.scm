(define dx 0.00001)

(define (average x y) (/ (+ x y) 2))

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f (+ x dx)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i r)
    (if (= i 1)
      r
      (iter (- i 1) (compose f r))))
  (iter n f))

(define (smooth-n f n)
  (repeated f n))

(define (f x) (* x x))

(display ((smooth f) 5))
(newline)