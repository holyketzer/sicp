(define (puts x)
  (display x)
  (newline))

(define (show-int i)
  (display (center i))
  (display "±")
  (display (radius i))
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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
    (add-interval (div-interval one r1)
                  (div-interval one r2)))))

(let ((interval1 (make-interval 100 101))
  (interval2 (make-interval 50 51)))

  (show-int (par1 interval1 interval2))
  (show-int (par2 interval1 interval2))
  (newline)

  (show-int (div-interval interval1 interval1))
  (show-int (div-interval interval1 interval2)))
