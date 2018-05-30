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

(define (union-set set1 set2)
  (define (iter res s1 s2)
    (cond
      ((null? s1) (append res s2))
      ((null? s2) (append res s1))
      (else
        (let (
          (s1next (car s1))
          (s2next (car s2)))

          (if (= s1next s2next)
            (iter (append res (list s1next)) (cdr s1) (cdr s2))
            (if (< s1next s2next)
              (iter (append res (list s1next)) (cdr s1) s2)
              (iter (append res (list s2next)) s1 (cdr s2))))))))

  (iter '() set1 set2))

(define s1 '(1 2 3 4))
(define s2 '(3 4 5))

(puts (union-set s1 s2))
