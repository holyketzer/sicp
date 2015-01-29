(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))

(* 9 9)

(define (mult a b)
  (mult-iter 0 a b))

(define (mult-iter x a b)
  (cond ((= b 0) x)
        ((even? b) (mult-iter x (double a) (halve b)))
        (else (mult-iter (+ x a) a (- b 1)))))

(mult 8 9)