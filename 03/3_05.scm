(define (puts x)
  (display x)
  (newline))

(define radius 10000)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0) (/ trials-passed trials))
      ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else (iter (- trials-remaining 1) trials-passed))))

  (iter trials 0.0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p bottom-left top-right trials)
  (let (
    (x1 (car bottom-left))
    (y1 (cdr bottom-left))
    (x2 (car top-right))
    (y2 (cdr top-right)))

    (define (experiment)
      (let (
        (x (random-in-range x1 x2))
        (y (random-in-range y1 y2)))

        (p x y)))

    (let ((rect-area (* (- x2 x1) (- y2 y1))))
      (* rect-area (monte-carlo trials experiment)))))

(define (square x)
  (* x x))

(define (circle-area-predicate x y)
  (<= (+ (square x) (square y)) (square radius)))

(define (estimate-pi trials)
  (let ((area (estimate-integral circle-area-predicate (cons (- radius) (- radius)) (cons radius radius) trials)))
    (/ area (square radius))))

(puts (estimate-pi 10))
(puts (estimate-pi 100))
(puts (estimate-pi 1000))
(puts (estimate-pi 10000))
(puts (estimate-pi 100000))
