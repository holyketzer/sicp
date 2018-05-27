(define (puts x)
  (display x)
  (newline))

(puts (list 'a 'b 'c))
;(a b c)

(puts (list (list 'george)))
;((george))

(puts (cdr '((x1 x2) (y1 y2))))
;((y1 y2))

(puts (cadr '((x1 x2) (y1 y2))))
;(y1 y2)

(puts (pair? (car '(a short list))))
;false

(puts (memq 'red '((red shoes) (blue socks))))
;false

(puts (memq 'red '(red shoes blue socks)))
;true

