(define (puts x)
  (display x)
  (newline))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v n)
  (make-vect (* n (xcor-vect v)) (* n (ycor-vect v))))

(define v1 (make-vect 1 2))
(define v2 (make-vect 2 3))

(puts (add-vect v1 v2))
(puts (sub-vect v1 v2))
(puts (scale-vect v1 3))
