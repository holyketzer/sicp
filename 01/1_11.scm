(define (puts x)
  (display x)
  (newline))

(define (f-rec n)
  (if (< n 3)
    n
    (+ (f-rec (- n 1)) (f-rec (- n 2)) (f-rec (- n 3)))))

(puts (f-rec 10))
(puts "")

(define (f-iter n)
  (define (iter a b c counter)
    (cond ((= counter n) (+ a b c))
      (else (iter (+ a b c) a b (+ counter 1)))))

  (if (< n 3)
    n
    (iter 2 1 0 3)))

(puts (f-iter 1))
(puts (f-iter 2))
(puts (f-iter 3))
(puts (f-iter 4))
(puts (f-iter 5))
(puts (f-iter 6))
(puts (f-iter 7))
(puts (f-iter 8))
(puts (f-iter 9))
(puts (f-iter 10))
