(define (equ? a b)
  (apply-generic 'equ? a b))

(define (ne a b)
  #f)

(put 'equ? '(complex rational) ne)
(put 'equ? '(complex scheme-number) ne)
(put 'equ? '(rational complex) ne)
(put 'equ? '(rational scheme-number) ne)
(put 'equ? '(scheme-number rational) ne)
(put 'equ? '(scheme-number complex) ne)

; scheme-number
(define (equ-numbers? a b)
  (= a b))

(put 'equ? '(scheme-number scheme-number) equ-numbers?)

; rational
(define (equ-rational? a b)
  (=
    (* (numer a) (denom b))
    (* (numer b) (denom a))))

(put 'equ? '(rational rational) equ-rational?)

; complex
(define (equ-complex? a b)
  (and
    (= (real-part a) (real-part b))
    (= (imag-part a) (imag-part b))))

(put 'equ? '(complex complex) equ-complex?)



