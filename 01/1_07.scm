(define
  (improve guess x)
  (average guess (/ x guess)))

(define
  (average x y)
  (/ (+ x y) 2))

(define
  (good-enough? guess x)
  (define (guessRes) (square guess))
  (define
    (precision)
    (if (> (guessRes) x)
        (/ x (guessRes))
        (/ (guessRes) x)))
  (> (* 100 (precision)) 99.99999))

(define
  (sqrt-iter guess x)
  (if
    (good-enough? guess x) guess
    (sqrt-iter (improve guess x) x)))

(display
  (sqrt-iter 1.0 2))
