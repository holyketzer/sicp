(define (puts x)
  (display x)
  (newline))

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT вызвана с пустой очередью" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
      ((empty-queue? queue)
        (set-front-ptr! queue new-pair)
        (set-rear-ptr! queue new-pair)
        queue)
      (else
        (set-cdr! (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair) queue))))

(define (delete-queue! queue)
  (cond
    ((empty-queue? queue) (error "DELETE! вызвана с пустой очередью" queue))
    (else (set-front-ptr! queue (cdr (front-ptr queue))) queue)))

(define q1 (make-queue))
(puts (insert-queue! q1 'a))
(puts (insert-queue! q1 'b))

(puts (delete-queue! q1))
(puts (delete-queue! q1))

(define (print-queue q)
  (display (front-ptr q))
  (newline))

(define q2 (make-queue))
(print-queue (insert-queue! q2 'a))
(print-queue (insert-queue! q2 'b))

(print-queue (delete-queue! q2))
(print-queue (delete-queue! q2))
