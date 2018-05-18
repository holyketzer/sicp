(define (puts x)
  (display x)
  (newline))

(define (reverse-r sequence)
  (fold-right (lambda (x res) (append res (list x))) '() sequence))

(define (reverse-l sequence)
  (fold-left (lambda (res x) (cons x res)) '() sequence))

(puts (reverse-r (list 1 2 3 4 5)))
(puts (reverse-l (list 1 2 3 4 5)))
