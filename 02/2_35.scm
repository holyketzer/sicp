(define (puts x)
  (display x)
  (newline))

(define (count-leaves x)
  (cond ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+
      (count-leaves (car x))
      (count-leaves (cdr x))))))

(define (count-leaves-with-reduce l)
  (define (reduce-subtree subtree)
    (if (pair? subtree)
      (count-leaves-with-reduce subtree)
      1))

  (reduce-left + 0 (map (lambda (subtree) (reduce-subtree subtree)) l)))

(define l (list 1 (list 2 (list 3 4))))

(puts (count-leaves l))
(puts (count-leaves-with-reduce l))

