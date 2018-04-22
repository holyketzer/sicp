(define (improve guess x)
  (/
    (+
      (/
        x
        (square guess))
      (* 2 guess))
    3))

(define (cube x)
  (* x x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (define (guessRes) (cube guess))
  (define
    (precision)
    (if (> (guessRes) x)
        (/ x (guessRes))
        (/ (guessRes) x)))
  (> (* 100 (precision)) 99.99999))

(define (cube-root-iter guess x)
  (if
    (good-enough? guess x) guess
    (cube-root-iter (improve guess x) x)))

(display
  (cube-root-iter 5.0 10.0))
