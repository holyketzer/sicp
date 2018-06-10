(define (=zero? poly)
  (all? (lambda (c) (=zero? c)) (term-list polu)))
