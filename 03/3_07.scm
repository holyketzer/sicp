(define (puts x)
  (display x)
  (newline))

(define (make-account balance current-password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount)) balance)
        "Недостаточно денег на счете"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch password m)
    (check-passsword password)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m))))

  (define (check-passsword password)
    (if (eq? current-password password)
      #t
      (error "Неверный пароль")))

  dispatch)

(define (make-joint acc password new-password)
  (define withdraw (acc password 'withdraw))

  (define deposit (acc password 'deposit))

  (define (dispatch password m)
    (check-passsword password)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m))))

  (define (check-passsword password)
    (if (eq? new-password password)
      #t
      (error "Неверный пароль")))

  dispatch)

(define acc (make-account 100 'secret))
(puts ((acc 'secret 'withdraw) 55))


(define same-acc (make-joint acc 'secret 'rosebud))
(puts ((same-acc 'rosebud 'withdraw) 5))
(puts ((acc 'secret 'withdraw) 5))

(puts ((same-acc 'secret 'withdraw) 5))
