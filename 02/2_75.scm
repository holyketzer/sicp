(define (puts x)
  (display x)
  (newline))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atan y x))
      (else (error "Неизвестная оп. -- MAKE-FROM-REAL-IMAG" op)))) dispatch)

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) (* m (cos a)))
      ((eq? op 'imag-part) (* m (sin a)))
      ((eq? op 'magnitude) m)
      ((eq? op 'angle) a)
      (else (error "Неизвестная оп. -- MAKE-FROM-REAL-IMAG" op)))) dispatch)

(define (apply-generic op arg)
  (arg op))

(puts (apply-generic 'real-part (make-from-mag-ang 1 2)))
