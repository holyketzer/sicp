(define (puts x)
  (display x)
  (newline))

(define (pputs l)
  (map (lambda (x) (puts x)) l)
  (newline))

(define (enumerate-interval a b)
  (define (iter res i)
    (if (< i a)
      res
      (iter (cons i res) (- i 1))))
  (iter '() b))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (and? a b)
  (and a b))

(define (all? l)
  (fold-right and? #t l))

(define (queens board-size)
  (define empty-board
    (list))

  (define (make-coords col row)
    (cons col row))

  (define (adjoin-position new-row k positions)
    (append positions (list (make-coords k new-row))))

  (define (get-col coords)
    (car coords))

  (define (get-row coords)
    (cdr coords))

  (define (safe-row? another)
    (lambda (queen)
      (let ((row (get-row queen)))
        (= 0 (length (filter (lambda (coords) (= row (get-row coords))) another))))))

  (define (safe-diag? another)
    (lambda (queen)
        (= 0 (length (filter (lambda (coords) (diag-eq? queen coords)) another)))))

  (define (diag-eq? a b)
    (let (
      (a-row (get-row a))
      (b-row (get-row b))
      (a-col (get-col a))
      (b-col (get-col b)))

      (= (abs (- a-row b-row)) (abs (- a-col b-col)))))

  (define (safe? col positions)
    (let (
      (queens (filter (lambda (coords) (= col (get-col coords))) positions))
      (rest (filter (lambda (coords) (not (= col (get-col coords)))) positions)))

      (and
        (< (length queens) 2)
        (all? (map (safe-row? rest) queens))
        (all? (map (safe-diag? rest) queens)))))


  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map
              (lambda (new-row) (adjoin-position new-row k rest-of-queens))
              (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

(pputs (queens 8))
