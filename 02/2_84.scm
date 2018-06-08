(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let (
            (type1 (car type-tags))
            (type2 (cadr type-tags))
            (a1 (car args))
            (a2 (cadr args)))

            (cond
              ((eq? type1 type2) (error "Нет метода для этих типов" (list op type-tags)))
              ((super? a1 a2) (apply-generic op a1 (raise a2)))
              ((super? a2 a1) (apply-generic op (raise a1) a2))
              (else (error "Нет метода для этих типов" (list op type-tags))))))))))

(define (super? super child)
  (let ((raised-child (raise child))))
    (if (null? raised-child)
      #f
      (if (eq? (type-tag super) (type-tag raised-child))
        #t
        (super? super raised-child))))
