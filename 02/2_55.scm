(define (puts x)
  (display x)
  (newline))

(puts (car ''abracadabra))

(puts (car (quote (quote abracadabra))))

