(define (puts x)
  (display x)
  (newline))

(define rand
  (let ((seed 0))
    (define (next value)
      (modulo (+ value 57) 3571))

    (define (generate)
      (set! seed (next seed))
      seed)

    (define (reset new-seed)
      (set! seed new-seed))

    (lambda (action)
      (cond
        ((eq? action 'generate) (generate))
        ((eq? action 'reset) reset)
        (else (error "unknown action"))))))

(puts (rand 'generate))
(puts (rand 'generate))
(puts (rand 'generate))

(puts ((rand 'reset) 55))
(puts (rand 'generate))
