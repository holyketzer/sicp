(define (puts x)
  (display x)
  (newline))

(define (mystery x)
  (define (loop x y)
    (puts x)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))

  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

(puts v) ; first v
(puts w) ; reversed v
