(define (puts x)
  (display x)
  (newline))

(define x (list 'a 'b))
(define y (list 'c 'd))

(define z (append x y))

(puts z)
; a b c d

(puts (cdr x))
; b
(define w (append! x y))

(puts z)
; a b c d

(puts (cdr x))
; b c d
