(define (inc x) (+ x 1))
(define (dec x) (- x 1))

; Recursive process
(define (rec-plus a b)
  (if (= a 0)
      b
      (inc (rec-plus (dec a) b))))

(display (rec-plus 10 1))
(newline)

; Iterative process
(define (iter-plus a b)
  (if (= a 0)
      b
      (iter-plus (dec a) (inc b))))

(display (iter-plus 10 1))
(newline)
