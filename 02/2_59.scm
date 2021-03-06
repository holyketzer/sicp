(define (puts x)
  (display x)
  (newline))

(define (element-of-set? x set)
  (cond ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (union-set set1 set2)
  (cond
    ((null? set2) set1)
    (else (union-set (adjoin-set (car set2) set1) (cdr set2)))))

(define set1 (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 '()))))
(define set2 (adjoin-set 4 (adjoin-set 3 (adjoin-set 2 '()))))

(puts set1)
(puts set2)
(puts (union-set set1 set2))
