(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (next-divisor n)
  (cond
    ((= n 2) 3)
    (else (+ n 2))))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next-divisor test-divisor)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " is prime! *** ")
  (display (* elapsed-time 1000))
  (display " us")
  #t)

(define (start-prime-test n start-time)
  (cond
    ((prime? n) (report-prime n (- (runtime) start-time)))
    (else #f)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (search-for-primes from to count)
  (let ((left (if (timed-prime-test from) (- count 1) count)))
    (if (and (< from to) (> left 0)) (search-for-primes (+ from 1) to left))))

(search-for-primes 100000000 100020000 5)