(define (puts x)
  (display x)
  (newline))

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+
      (count-pairs (car x))
      (count-pairs (cdr x))
      1)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; 3
(puts (count-pairs '(1 2 3)))

; 4
(define x (list 2 3))
(define y (list 1))
(set-car! x y)
(set-car! (cdr x) y)
(puts (count-pairs x))

; Inf
(define z (make-cycle (list 'a 'b 'c)))
(puts (count-pairs z))


