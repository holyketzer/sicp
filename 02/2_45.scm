(define (puts x)
  (display x)
  (newline))

(define (below painter)
  painter)

(define (beside painter)
  painter)

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (split transform1 transform2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split transform1 transform1) painter (- n 1))))
        (transform1 painter (transform2 smaller smaller))))))

(define right-split-2 (split beside below))

(define up-split-2 (split below beside))

(puts right-split-2)
(puts up-split-2)
