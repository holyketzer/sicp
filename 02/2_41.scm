(define (puts x)
  (display x)
  (newline))

(define (unique-threes n)
  (flatmap
    (lambda (i)
      (flatmap
        (lambda (j)
          (map
            (lambda (k) (list i j k))
            (enumerate-interval 1 (- j 1))))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (enumerate-interval a b)
  (define (iter res i)
    (if (< i a)
      res
      (iter (cons i res) (- i 1))))
  (iter '() b))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (sum-eq? s)
  (lambda (list) (= s (fold-right + 0 list))))

(define (threes-sum-eq n s)
  (filter (sum-eq? s) (unique-threes n)))

(puts (unique-threes 7))
(puts (threes-sum-eq 7 10))
