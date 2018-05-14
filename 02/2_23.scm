(define (for-each proc items)
  (map proc items)
  '())

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
