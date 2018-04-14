(define
  (min a b)
  (if (< a b) a b))

(define
  (min3 a b c)
  (min a (min b c)))

(define
  (square x)
  (* x x))

(define
  (square-sum a b)
  (+ (square a) (square b)))

(define
  (square-sum-of-max-couple a b c)
  (cond
    ((= (min3 a b c) a) (square-sum b c))
    ((= (min3 a b c) b) (square-sum a c))
    ((= (min3 a b c) c) (square-sum a b))))

(display (square-sum-of-max-couple 10 2 3))
