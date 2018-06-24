(define (or-gate a1 a2 output)
  (let (
    (m1 (make-wire))
    (m2 (make-wire))
    (o (make-wire)))

    (inverter a1 m1)
    (inverter a2 m2)
    (and-gate m1 m2 o)
    (inverter o output)

    'ok))

; delay = and-delay + (2 * inverter-delay)
