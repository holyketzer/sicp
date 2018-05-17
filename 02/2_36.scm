(define (puts x)
  (display x)
  (newline))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (reduce-left op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define l '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(puts (accumulate-n + 0 l))
