(define (puts x)
  (display x)
  (newline))

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

(define (make-rect p1 p2)
  (cons p1 p2))

(define (rect-corner-1 rect)
  (car rect))

(define (rect-corner-2 rect)
  (cdr rect))

(define (rect-size rect coord-picker)
  (abs (- (coord-picker (rect-corner-1 rect)) (coord-picker (rect-corner-2 rect)))))

(define (rect-length rect)
  (rect-size rect x-point))

(define (rect-width rect)
  (rect-size rect y-point))

(define (rect-area rect)
  (* (rect-length rect) (rect-width rect)))

(define (rect-perimiter rect)
  (* 2 (+ (rect-length rect) (rect-width rect))))

(let
  ((rect (make-rect (make-point 0 0) (make-point 7 3))))

  (puts (rect-area rect))
  (puts (rect-perimiter rect)))
