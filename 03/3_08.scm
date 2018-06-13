(define (puts x)
  (display x)
  (newline))

(define f
  (let ((value 0))
    (lambda (x)
      (let ((prev value))
        (set! value x)
        prev))))


(puts (+ (f 0) (f 1)))
(puts (+ (f 1) (f 0)))
