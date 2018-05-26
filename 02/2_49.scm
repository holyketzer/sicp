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

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (caddr frame))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (draw-frame frame)
  (let (
    (o (origin frame))
    (e1 (edge1 frame))
    (e2 (edge2 frame)))
      (let (
        (oe1 (add-vect o e1))
        (oe2 (add-vect o e2))
        (oe12 (add-vect o (add-vect e1 e2))))

        (let (
          (segment-list
            (list
              (make-segment o oe1)
              (make-segment oe1 oe12)
              (make-segment oe12 oe2)
              (make-segment oe2 o)))

          ((segments->painter segment-list) frame))))))
