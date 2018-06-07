(define (apply-generic op . args)
  (define (get-coercion-to to-type)
    (lambda (from-type)
      (if (eq? from-type to-type)
        identity
        (get-coercion from-type to-type))))

  (define (convert ltype-tags rtype-tags)
    (if (null? rtype-tags)
      (error "Нет метода для этих типов" (list op ltype-tags))
      (let (
        (tag (car rtype-tags))
        (rest-tags (cdr rtype-tags)))

        (let (
          (ltype-tags-coercions (map (get-coercion-to tag) ltype-tags))
          (tag-coersion (lambda (x) x))
          (rtype-tags-coercions (map (get-coercion-to tag) rtype-tags))
          (converted-tags (map (lambda (x) (tag) args))))

          (let (
            (coersions (append ltype-tags-coercions (list tag-coersion) rtype-tags-coercions))
            (proc (get op converted-tags)))

            (if (and (not-null? proc) (every (lambda (c) (not-null?)) coersions))
              (apply-generic op (map (lambda (v c) (c v)) args coersions))
              (convert (append ltype-tags (list tag)) rest-tags)))))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (convert '() type-tags)))))
