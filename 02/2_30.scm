(define (puts x)
  (display x)
  (newline))

(define (square-tree tree)
  (cond ((null? tree) '())
    ((not (pair? tree)) (square tree))
    (else (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
    (if (pair? sub-tree)
      (square-tree-2 sub-tree)
      (square sub-tree)))
    tree))

(define l
  (list 1
    (list 2 (list 3 4) 5)
    (list 6 7)))

(puts l)
(puts (square-tree l))
(puts (square-tree-2 l))
