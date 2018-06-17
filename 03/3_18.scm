(define (puts x)
  (display x)
  (newline))

(define (have-cycle? x)
  (let ((counted '()))
    (define (counted? p)
      (memq p counted))

    (define (mark-counted p)
      (set! counted (cons p counted)))

    (define (have-cycle-iter xx)
      (cond
        ((not (pair? xx)) #f)
        ((counted? xx) #t)
        (else
          (mark-counted xx)
          (or (have-cycle-iter (car xx)) (have-cycle-iter (cdr xx))))))

    (have-cycle-iter x)))


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
