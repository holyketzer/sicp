(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; factorial n=6
; factorial n=5
; factorial n=4
; factorial n=3
; factorial n=2
; factorial n=1

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product) (+ counter 1) max-count)))

(define (factorial n)
  (fact-iter 1 1 n))

; factorial n=6
; fact-iter product=1 counter=1 max-count=6
; fact-iter product=1 counter=2 max-count=6
; fact-iter product=2 counter=3 max-count=6
; fact-iter product=6 counter=4 max-count=6
; fact-iter product=24 counter=5 max-count=6
; fact-iter product=120 counter=6 max-count=6
; fact-iter product=720 counter=7 max-count=6
