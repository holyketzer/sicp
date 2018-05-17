(define (puts x)
  (display x)
  (newline))

(define (puts-matrix m)
  (map puts m)
  (newline))

(define (append-item list1 item)
  (if (null? list1)
    (list item)
    (cons (car list1) (append-item (cdr list1) item))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (fold-right op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (reduce-left + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mv) (dot-product v mv)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define v '(1 2 3 4))
(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define n '((1 2 3) (4 5 6) (6 7 8) (4 6 9)))

(puts-matrix m)

(puts (dot-product v v))
(puts (matrix-*-vector m v))
(puts-matrix (transpose m))
(puts-matrix (matrix-*-matrix m n))
