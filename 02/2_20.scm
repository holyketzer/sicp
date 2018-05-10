(define (puts x)
  (display x)
  (newline))

;(define (even? x)
;  (= (/ x 2) 0))

;(define (odd? x)
;  (= (/ x 2) 1))

(define (append list1 item)
  (if (null? list1)
    (list item)
    (cons (car list1) (append (cdr list1) item))))

(define (filter predicate l)
  (define (iter l res)
    (if (null? l)
      res
      (let ((next (car l)))
        (if (predicate next)
          (iter (cdr l) (append res next))
          (iter (cdr l) res)))))

  (iter l (list)))

(define (same-parity x . l)
  (cons x (if (even? x)
    (filter even? l)
    (filter odd? l))))

(puts (same-parity 1 2 3 4 5 6 7))
(puts (same-parity 2 3 4 5 6 7))
