(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a acc)
    (if (> a b)
      acc
      (iter (next a) (combiner acc (if (predicate a) (term a) null-value)))))
  (iter a null-value))

(define (sum-of-primes-square n)
  (filtered-accumulate + 0 square 0 inc n prime?))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (show fn x)
  (newline)
  (display (fn x)))

(show sum-of-primes-square 10)
(show sum-of-primes-square 100)