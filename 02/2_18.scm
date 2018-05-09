(define (puts x)
  (display x)
  (newline))

(define (append list1 item)
  (if (null? list1)
    (list item)
    (cons (car list1) (append (cdr list1) item))))

(define (reverse l)
  (if (null? l)
    l
    (append (reverse (cdr l)) (car l))))

(puts (reverse (list 1 2 3 4 5)))
