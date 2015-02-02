(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " is prime! *** ")
  (display (* elapsed-time 1000))
  (display " us")
  #t)

(define (start-prime-test n start-time)
  (cond
    ((fast-prime? n 100) (report-prime n (- (runtime) start-time)))
    (else #f)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (search-for-primes from to count)
  (let ((left (if (timed-prime-test from) (- count 1) count)))
    (if (and (< from to) (> left 0)) (search-for-primes (+ from 1) to left))))

(search-for-primes 100000000000000000 100000000000020000 5)