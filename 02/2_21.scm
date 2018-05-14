(define (puts x)
  (display x)
  (newline))

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
    items
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))

(puts (square-list (list 1 2 3 4 5)))
(puts (square-list-2 (list 1 2 3 4 5)))
