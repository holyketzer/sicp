(define (complex->real r)
  (if (= 0 (imag-part (contents r)))
    (make-real (real-part (contents r)) 0)
    '()))

(define (real->rational r)
  (let ((content (contents n)))
    (make-rat (real-numer (contents n)) (real-denom (contents n)))))

(define (scheme-number->rational n)
  (if (number? (contents n))
    (contents n)
    '()))

(put-coercion 'complex 'real complex->real)
(put-coercion 'real 'rational real->rational)
(put-coercion 'rational 'scheme-number rational->scheme-number)

(define (drop x)
  (let ((type-tag (type-tag x)))
    (let ((supertype (get 'raise type-tag)))
      (if supertype
        (let (dropped ((get-coercion type-tag supertype) x))
          (if (null? dropped)
            x
            (drop dropped)))))))
