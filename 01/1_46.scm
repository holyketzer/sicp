(define tolerance 0.00001)

(define (puts x)
  (display x)
  (newline))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) tolerance))

  (sqrt-iter 1.0 x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))

  (try first-guess))

(define (iterative-improve good-enough? improve)
  (define (iter value)
    (let ((next-value (improve value)))
      (if (good-enough? value next-value)
          next-value
          (iter next-value))))

  iter)

(define (sqrt2 x)
  (define (good-enough? guess next-guess)
    (< (abs (- (square next-guess) x)) tolerance))

  (define (improve guess)
    (average guess (/ x guess)))

  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point2 f)
  (define (good-enough? guess next-guess)
    (< (abs (- guess next-guess)) tolerance))

  (define (improve guess)
    (average guess (f guess)))

  ((iterative-improve good-enough? improve) 1.0))

(puts (sqrt 5))
(puts (sqrt2 5))

;(puts (fixed-point (lambda (y) (/ 5 y)) 1.0)) doesn't work without average damp
(puts (fixed-point2 (lambda (y) (/ 5 y))))
