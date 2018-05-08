(define (puts x)
  (display x)
  (newline))

(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let
    ((p1 (* (lower-bound x) (lower-bound y)))
      (p2 (* (lower-bound x) (upper-bound y)))
      (p3 (* (upper-bound x) (lower-bound y)))
      (p4 (* (upper-bound x) (upper-bound y))))

    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((uy (upper-bound y))
    (ly (lower-bound y)))

    (if (or (= 0 (* uy ly)) (and (> ly 0) (< uy 0)) (and (< ly 0) (> uy 0)))
      (error "divider cross zero")
      (mul-interval x
        (make-interval
          (/ 1.0 uy)
          (/ 1.0 ly))))))

(define (radius interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (let ((w (/ (* c p) 100.0)))
    (make-interval (- c w) (+ c w))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent interval)
  (* 100.0 (/ (radius interval) (center interval))))

(puts (percent (make-interval 2 3)))
(puts (percent (make-interval 5 7)))
(puts (percent (make-interval 10 11)))
