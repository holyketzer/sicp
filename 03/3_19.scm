(define (puts x)
  (display x)
  (newline))

(define (have-cycle? x)
  (define (safe-cdr l)
     (if (pair? l)
         (cdr l)
         '()))

  (define (have-cycle-iter prev curr)
    (cond
      ((not (pair? prev)) #f)
      ((not (pair? curr)) #f)
      ((eq? prev curr) #t)
      ((eq? prev (safe-cdr curr)) #t)
      (else (have-cycle-iter (safe-cdr prev) (safe-cdr (safe-cdr curr))))))

  (have-cycle-iter (safe-cdr x) (safe-cdr (safe-cdr x))))


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; 3
(puts (have-cycle? '(1 2 3)))

; 4
(define x (list 2 3))
(define y (list 1))
(set-car! x y)
(set-car! (cdr x) y)
(puts (have-cycle? x))

; Inf
(define z (make-cycle (list 'a 'b 'c)))
(puts (have-cycle? z))
