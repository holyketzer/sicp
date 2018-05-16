(define (puts x)
  (display x)
  (newline))

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (ss) (cons (car s) ss)) rest)))))

(puts (subsets (list 1 2 3)))
