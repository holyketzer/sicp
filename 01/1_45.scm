(define tolerance 0.00001)

(define (puts x)
  (display x)
  (newline))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f step first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess)
    ;(puts guess)
    (let ((next (step f guess)))
      (if (close-enough? guess next)
        next
        (try next))))

  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i r)
    (if (= i 1)
      r
      (iter (- i 1) (compose f r))))
  (iter n f))

(define (log2 x)
  (/ (log x) (log 2)))

(define (next-step n)
  (lambda (f guess)
    (define (damp-count)
      (floor (log2 n)))
    (((repeated average-damp (max 1 (damp-count))) f) guess)))

(define (nth n)
  (cond
    ((= n 0) (lambda (x) 1))
    ((= n 1) (lambda (x) x))
    (else (lambda (x) (* x ((nth (- n 1)) x))))))

(define (nthroot value n)
  (define (f)
    (lambda (y) (/ value ((nth (- n 1)) y))))
  (fixed-point (f) (next-step n) (/ value 2.0)))

(puts (nthroot 9 1))
(puts (nthroot 9 2))
(puts (nthroot 9 3))
(puts (nthroot 9 4))
(puts (nthroot 9 5))
(puts (nthroot 9 6))
(puts (nthroot 9 7))
(puts (nthroot 9 8))
(puts (nthroot 9 9))
(puts (nthroot 9 10))
(puts (nthroot 9 11))
(puts (nthroot 9 100))
