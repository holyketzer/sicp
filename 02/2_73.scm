(define (puts x)
  (display x)
  (newline))

(define (get on op proc)
  body)

(define (put on op proc)
  body)

(define (deriv exp var)
   (cond ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (variable? x)
  (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (install-deriv-package)
  (define (addend s)
    (cadr s))

  (define (augend s)
    (caddr s))

  (define (make-sum a1 a2)
    (cond
      ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2)) (+ a1 a2))
      (else (list '+ a1 a2))))

  (define (multiplier p)
    (cadr p))

  (define (multiplicand p)
    (caddr p))

  (define (make-product m1 m2)
    (cond
      ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2)) (else (list '* m1 m2))))

  (define (base e)
    (cadr e))

  (define (exponent e)
    (caddr e))

  (define (make-exponentiation base n)
    (cond
      ((=number? n 0) 1)
      ((=number? n 1) base)
      ((and (number? n) (number? base)) (expt base n))
      (else (list '** base n))))

  (define (sum exp var)
    (make-sum
      (deriv (addend exp) var)
      (deriv (augend exp) var)))

  (define (product exp var)
    (make-sum
      (make-product
        (multiplier exp)
        (deriv (multiplicand exp) var))
      (make-product
        (deriv (multiplier exp) var)
        (multiplicand exp))))

  (define (exponentiation exp var)
    (let (
      (n (exponent exp))
      (b (base exp)))
        (make-product
          n
          (make-exponentiation b (make-sum n -1)))))

  (put 'deriv '+ sum)
  (put 'deriv '* product)
  (put 'deriv '** exponentiation)

  'done)

(install-deriv-package)
