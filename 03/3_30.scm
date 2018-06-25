(define (full-adder a b c-in sum c-out)
  (let (
    (s (make-wire))
    (c1 (make-wire))
    (c2 (make-wire)))

    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define zero-wire (make-wire))

(define (riple-carry-adder a-wires b-wires s-wires c-out)
  (let (
    (curr-a (car a-wires))
    (curr-b (car b-wires))
    (curr-s (car s-wires))

    (rest-a (cdr a-wires))
    (rest-b (cdr b-wires))
    (rest-s (cdr s-wires)))

    (if (null? rest-a)
      (full-adder curr-a curr-b zero-wire curr-s c-out)
      (let ((c (make-wire)))
        (full-adder curr-a curr-b c curr-s c-out)
        (riple-carry-adder rest-a rest-b rest-s c)))))

; deplay = delay(summator) * n
; delay(summator) = 2 * delay(half-summator) + or-delay
; delay(half-summator) = max(or-delay, (and-delay + inverter-delay)) + and-delay
