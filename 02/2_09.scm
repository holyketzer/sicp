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
  (mul-interval x
    (make-interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y)))))

(define (radius interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0))

(define (print-op-and-radius operator interval1 interval2)
  (let ((result (operator interval1 interval2)))
    (puts result)
    (puts (radius interval1))
    (puts (radius interval2))
    (puts (radius result))
    (puts "")))

(let ((interval1 (make-interval 1 5))
  (interval2 (make-interval 2 3)))

  (print-op-and-radius add-interval interval1 interval2)
  (print-op-and-radius sub-interval interval1 interval2)
  (print-op-and-radius mul-interval interval1 interval2)
  (print-op-and-radius div-interval interval1 interval2))
