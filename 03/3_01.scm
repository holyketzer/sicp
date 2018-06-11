(define (puts x)
  (display x)
  (newline))

(define (make-accumulator res)
  (lambda (x)
    (begin
      (set! res (+ res x))
      res)))

(define a (make-accumulator 20))

(puts a)
(puts (a 5))
(puts (a 5))
(puts (a 15))
