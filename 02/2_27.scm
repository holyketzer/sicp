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

(define (deep-reverse l)
  (if (list? l)
    (if (null? l)
      l
      (append (deep-reverse (cdr l)) (deep-reverse (car l))))
    l))

(define x (list (list 1 2) (list 3 4)))

(puts (reverse x))
(puts (deep-reverse x))
