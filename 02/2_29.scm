(define (puts x)
  (display x)
  (newline))

(define (identity x)
  x)

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (map-branch branch mobile-case weight-case)
  (let (
      (structure (branch-structure branch)))

      (if (pair? structure)
        (mobile-case structure)
        (weight-case structure))))

(define (total-weight mobile)
  (define (branch-weight branch)
    (map-branch branch total-weight identity))

  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (torque branch)
  (let (
      (length (branch-length branch)))

      (* length (map-branch branch total-weight identity))))

(define (balanced-branch? branch)
  (map-branch branch balanced? (lambda (x) #t)))

(define (balanced? mobile)
  (let (
    (left (left-branch mobile))
    (right (right-branch mobile)))

    (and
      (= (torque left) (torque right))
      (balanced-branch? left)
      (balanced-branch? right))))


(define mobile (make-mobile (make-branch 2 27) (make-branch 2 (make-mobile (make-branch 5 12) (make-branch 4 15)))))

(puts mobile)
(puts (left-branch mobile))
(puts (right-branch mobile))
(puts (total-weight mobile))
(puts (balanced? mobile))
