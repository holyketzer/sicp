(define (puts x)
  (display x)
  (newline))

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (if (symbol? x)
        (cond
          ((eq? x 'how-many-calls?) count)
          ((eq? x 'reset-count) (set! count 0))
          (else (error "unknown operation")))
        (begin
          (set! count (+ 1 count))
          (f x))))))

(define s (make-monitored sqrt))

(puts (s 100))
(puts (s 25))
(puts (s 'how-many-calls?))

(puts (s 'reset-count))
(puts (s 'how-many-calls?))
(puts (s 100))
(puts (s 'how-many-calls?))
