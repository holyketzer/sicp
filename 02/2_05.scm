(define (puts x)
  (display x)
  (newline))

(define (remainder a n)
  (- a (* n (floor (/ a n)))))

(define (remainder-rest a n)
  (define (iter rest)
    (let ((current (remainder rest n)))
      (cond
        ((= rest 0) 0)
        ((= current 0) (iter (/ rest n)))
        (else rest))))

  (iter a))

(define (power x n)
  (cond
    ((= n 0) 1)
    ((= n 1) x)
    (else (* x (power x (- n 1))))))

(define (logn n x)
  (/ (log x) (log n)))

(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (car c)
  (logn 2 (remainder-rest c 3)))

(define (cdr c)
  (logn 3 (remainder-rest c 2)))

(puts (power 2 3))
(puts (power 3 10))
(puts (cons 3 10))
(puts (remainder-rest (cons 3 10) 2))
(puts (remainder-rest 0 2))
(puts (car (cons 3 10)))
(puts (cdr (cons 3 10)))
