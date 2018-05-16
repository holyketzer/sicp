(define (puts x)
  (display x)
  (newline))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x acc) (cons (p x) acc)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x acc) (+ 1 acc)) 0 sequence))

(puts (map square (list 1 2 3 4 5)))
(puts (append (list 1 2 3 4 5) (list 6 7 8)))
(puts (length (list 1 2 3 4 5)))
