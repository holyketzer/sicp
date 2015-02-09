(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(newline)
(display (integral cube 0 1 0.01))
(newline)
(display (integral cube 0 1 0.001))

(define (integral-simp f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (define (next x) (+ x 1))

  (* (/ h 3) (sum term 0 next n)))

(newline)
(newline)
(display (integral-simp cube 0 1 100.0))
(newline)
(display (integral-simp cube 0 1 1000.0))