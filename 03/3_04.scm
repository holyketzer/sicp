(define (puts x)
  (display x)
  (newline))

(define (make-account balance current-password)
  (let ((wrong-password-counter 0))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount)) balance)
          "Недостаточно денег на счете"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (dispatch password m)
      (if (check-password password)
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m)))
        (lambda (x) "Неверный пароль")))

    (define (check-password password)
      (if (eq? current-password password)
        #t
        (begin
          (set! wrong-password-counter (+ 1 wrong-password-counter))
          (if (> wrong-password-counter 6)
            (call-the-cops)
            #f))))

    (define (call-the-cops)
      (error "Call the cops!"))

    dispatch))

(define acc (make-account 100 'secret))

(puts ((acc 'secret 'withdraw) 55))
(puts ((acc 'secret 'withdraw) 10))

(puts ((acc 'wrong 'withdraw) 1))
(puts ((acc 'wrong 'withdraw) 1))
(puts ((acc 'wrong 'withdraw) 1))
(puts ((acc 'wrong 'withdraw) 1))
(puts ((acc 'wrong 'withdraw) 1))
(puts ((acc 'wrong 'withdraw) 1))
(puts ((acc 'wrong 'withdraw) 1))
