(define (puts x)
  (display x)
  (newline))

(define (present? l)
  (not (null? l)))

(define (my-equal? a b)
  (puts a)
  (puts b)
  (cond
    ((and (symbol? a) (symbol? b)) (eq? a b))
    ((and (pair? a) (pair? b))
      (cond
        ((and (present? a) (present? b)) (and (my-equal? (car a) (car b)) (my-equal? (cdr a) (cdr b))))
        ((and (null? a) (null? b)) #t)
        (else #f)))
    (else #f)))

(puts (equal? '(this is a list) '(this is a list)))
(puts (equal? '(this is a list) '(this (is a) list)))

(puts (my-equal? '(this is a list) '(this is a list)))
(puts (my-equal? '(this is a list) '(this (is a) list)))
