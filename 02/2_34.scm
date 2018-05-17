(define (puts x)
  (display x)
  (newline))

(define (horner-eval x coefficient-sequence)
  (reduce-left (lambda (higher-terms this-coeff) (+ this-coeff (* x higher-terms)))
    0
    coefficient-sequence))

(puts (horner-eval 2 (list 1 3 0 5 0 1)))
