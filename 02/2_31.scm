(define (puts x)
  (display x)
  (newline))

(define (tree-map tree mapper)
  (map (lambda (sub-tree)
    (if (pair? sub-tree)
      (tree-map sub-tree mapper)
      (mapper sub-tree)))
    tree))

(define (square-tree tree)
  (tree-map tree square))

(define l
  (list 1
    (list 2 (list 3 4) 5)
    (list 6 7)))

(puts l)
(puts (square-tree l))
