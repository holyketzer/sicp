(define (puts x)
  (display x)
  (newline))

(define (element-of-set? x set)
  (cond ((null? set) #f)
  ((= x (car set)) #t)
  ((< x (car set)) #f)
  (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (define (insert x left right)
    (cond
      ((null? right) (append left (list x)))
      ((= x (car right)) (append left right))
      ((< x (car right)) (append left (list x) right))
      (else (insert x (append left (list (car right))) (cdr right)))))

  (insert x '() set))

(puts (adjoin-set 4 (adjoin-set 2 (adjoin-set 5 (adjoin-set 1 (adjoin-set 3 '()))))))
