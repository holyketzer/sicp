(define (scheme-number->rational n)
  (make-rational (contents n) 0))

(define (rational->real r)
  (let ((content (contents n)))
    (make-real (/ (car content) (cdr content)))))

(define (real-complex r)
  (make-complex (contents r) 0))

(put-coercion 'scheme-number 'rational scheme-number->rational)
(put-coercion 'rational 'real rational->real)
(put-coercion 'real 'complex real-complex)

(define (raise x)
  (let ((type-tag (type-tag x)))
    (let ((supertype (get 'raise type-tag)))
      (if supertype
        ((get-coercion type-tag supertype) x)
          '()))))
