(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (insert-at (order term) (coeff term) term-list)))

(define (insert-at index item list)
  (if (= (order term) (length list))
    (cons item list))
    (insert-at index item (cons 0 list)))

(define (the-empty-termlist)
  '())

(define (first-term term-list)
  (list (- (length term-list) 1) (car term-list)))

(define (rest-terms term-list)
  (cdr term-list))

(define (empty-termlist? term-list)
  (null? term-list))

(define (make-term order coeff)
  (list order coeff))

(define (order term)
  (car term))

(define (coeff term)
  (cadr term))
