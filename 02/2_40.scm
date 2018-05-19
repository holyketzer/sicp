(define (puts x)
  (display x)
  (newline))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map
        (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))


(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0))

  (define (find-divisor n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (+ test-divisor 1)))))

  (define (smallest-divisor n)
    (find-divisor n 2))

  (= n (smallest-divisor n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (enumerate-interval a b)
  (define (iter res i)
    (if (< i a)
      res
      (iter (cons i res) (- i 1))))
  (iter '() b))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (prime-sum-pairs n)
  (map make-pair-sum
    (filter prime-sum? (unique-pairs n))))

(puts (enumerate-interval 1 10))
(puts (prime-sum-pairs 5))
