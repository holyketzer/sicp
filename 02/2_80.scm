(define (=zero? a)
  (apply-generic '=zero? a))

; scheme-number
(define (=zero-numbers? a)
  (= 0 a))

(put '=zero? '(scheme-number) =zero-numbers?)

; rational
(define (=zero-rational? a)
  (= 0 (numer a)))

(put '=zero? '(rational) =zero-rational?)

; complex
(define (=zero-complex? a)
  (and
    (= 0 (real-part a))
    (= 0 (imag-part a))))

(put '=zero? '(complex) =zero-complex?)
