(define (average x y) (/ (+ x y) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess steps)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (cond
        ((close-enough? guess next)
          (display steps)
          (display " steps")
          (newline)
          next)
        (else (try next (+ steps 1))))))

  (try first-guess 0))

(display "Simple fixed-point")
(newline)
(display (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
(newline)
(newline)

(display "With average damping")
(newline)
(display (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))
(newline)
(newline)