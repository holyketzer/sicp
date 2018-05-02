(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-point segment)
  (car segment))

(define (end-point segment)
  (cdr segment))

(define (average x y)
  (/ (+ x y) 2))

(define (mid-point segment)
  (let
    ((start (start-point segment))
    (end (end-point segment)))

    (let
      ((x1 (x-point start))
      (y1 (y-point start))
      (x2 (x-point end))
      (y2 (y-point end)))

      (make-segment (average x1 x2) (average y1 y2)))))

(print-point (mid-point (make-segment (make-point 1.0 2.0) (make-point 4.0 5.0))))
(print-point (mid-point (make-segment (make-point 1.0 2.0) (make-point 30.0 40.0))))
