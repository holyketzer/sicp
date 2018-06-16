(define (puts x)
  (display x)
  (newline))

(define (count-pairs x)
  (let ((counted '()))
    (define (counted? p)
      (define (iter rest)
        (cond
          ((null? rest) #f)
          ((eq? p (car rest)) #t)
          (else (iter (cdr rest)))))

      (iter counted))

    (define (mark-counted p)
      (set! counted (cons p counted)))

    (define (count-iter xx)
      (cond
        ((not (pair? xx)) 0)
        ((counted? xx) 0)
        (else
          (mark-counted xx)
          (+
            (count-iter (car xx))
            (count-iter (cdr xx))
            1))))

    (count-iter x)))


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; 3
(puts (count-pairs '(1 2 3)))

; 4
(define x (list 2 3))
(define y (list 1))
(set-car! x y)
(set-car! (cdr x) y)
(puts (count-pairs x))

; Inf
(define z (make-cycle (list 'a 'b 'c)))
(puts (count-pairs z))


