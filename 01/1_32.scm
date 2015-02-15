(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (accumulate-rec combiner null-value term (next a) next b) (term a))))

(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
      acc
      (iter (next a) (combiner acc (term a)))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (fact n)
  (product identity 1 inc n))

(define (sum-one n)
  (sum identity 1 inc n))

(define (show fn n)
  (define (iter x)
    (newline)
    (display (fn x))
    (if (< x n) (iter (+ x 1)) (newline)))
  (iter 1))

(show sum-one 10)
(show fact 10)