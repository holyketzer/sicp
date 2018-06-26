(define (averager a b c)
  (let (
    (s (make-connector))
    (d (make-connector)))

    (adder a b s)
    (multiplier s d c)
    (constant 0.5 d)
    'ok))
