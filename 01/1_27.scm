; classic prime

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

; fast prime

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (ferma-test n a)
  (cond
    ((= a 0) true)
    ((= (expmod a n n) a) (ferma-test n (- a 1)))
    (else false)))

(define (full-ferma-test n)
  (newline)
  (display n)
  (if (ferma-test n (- n 1))
    (if (prime? n)
      (display " it's really prime")
      (display " ferma test fail! It isn't prime!!!"))
    (display " is not prime")))

(full-ferma-test 1)
(full-ferma-test 4)
(full-ferma-test 17)
(full-ferma-test 50)
(full-ferma-test 561)
(full-ferma-test 1105)
(full-ferma-test 1729)
(full-ferma-test 2465)
(full-ferma-test 2821)
(full-ferma-test 6601)