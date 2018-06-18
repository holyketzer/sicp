(define (puts x)
  (display x)
  (newline))

(define (make-queue)
  (let (
    (front-ptr '())
    (rear-ptr '()))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty-queue?)
      (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT вызвана с пустой очередью")
        (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond
          ((empty-queue?)
            (set-front-ptr! new-pair)
            (set-rear-ptr! new-pair)
            dispatch)
          (else
            (set-cdr! rear-ptr new-pair)
            (set-rear-ptr! new-pair)
            dispatch))))

    (define (delete-queue!)
      (cond
        ((empty-queue?) (error "DELETE! вызвана с пустой очередью"))
        (else (set-front-ptr! (cdr front-ptr)))))

    (define (print-queue)
      (display front-ptr)
      (newline))

    (define (dispatch m)
      (cond
        ((eq? 'empty? m) empty-queue?)
        ((eq? 'front-queue m) front-queue)
        ((eq? 'insert-queue! m) insert-queue!)
        ((eq? 'delete-queue! m) delete-queue!)
        ((eq? 'print-queue m) print-queue)
        (else (error "Unknown operation " m))))

    dispatch))

(define q (make-queue))

((q 'print-queue))
(puts ((q 'empty?)))

((q 'insert-queue!) 1)
((q 'insert-queue!) 2)
((q 'insert-queue!) 3)
((q 'print-queue))
(puts ((q 'empty?)))

((q 'delete-queue!))
((q 'delete-queue!))
((q 'print-queue))
