(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (product-integers a b)
  (product identity a inc b))

(define (fact n)
  (product identity 1 inc n))

(define (pi-dw n)
  (define (x k)
    (if (odd? k) (+ k 1) (+ k 2)))
  (define (y k)
    (define kk (+ k 1))
    (if (even? kk) (+ kk 1) kk))
  (define (term k) (/ (x k) (y k)))
  (* 4.0 (product term 1 inc n)))

(newline)
(display (fact 6))

(newline)
(newline)
(display (pi-dw 10))
(newline)
(display (pi-dw 100))
(newline)
(display (pi-dw 1000))
(newline)
(display (pi-dw 10000))