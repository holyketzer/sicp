(define (puts x)
  (display x)
  (newline))

(define (set-cadr! pair value)
  (let ((rest (cdr pair)))
    (set-car! rest value)
    pair))

(define (set-cddr! pair value)
  (let ((rest (cdr pair)))
    (set-cdr! rest value)
    pair))

(define (make-dequeue)
  (let (
    (front-ptr '())
    (rear-ptr '()))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (empty?)
      (null? front-ptr))

    (define (front)
      (if (empty?)
        (error "FRONT вызвана с пустой очередью")
        (car front-ptr)))

    (define (rear)
      (if (empty?)
        (error "REAR вызвана с пустой очередью")
        (car rear-ptr)))

    (define (front-insert! item)
      (let ((new-pair (cons item (cons '() '()))))
        (cond
          ((empty?)
            (set-front-ptr! new-pair)
            (set-rear-ptr! new-pair)
            dispatch)
          (else
            (set-cadr! new-pair front-ptr)
            (set-cddr! front-ptr new-pair)
            (set-front-ptr! new-pair)
            dispatch))))

    (define (rear-insert! item)
      (let ((new-pair (cons item (cons '() '()))))
        (cond
          ((empty?)
            (set-front-ptr! new-pair)
            (set-rear-ptr! new-pair)
            dispatch)
          (else
            (set-cadr! rear-ptr new-pair)
            (set-cddr! new-pair rear-ptr)
            (set-rear-ptr! new-pair)
            dispatch))))

    (define (front-delete!)
      (cond
        ((empty?) (error "DELETE! вызвана с пустой очередью"))
        (else (set-front-ptr! (cadr front-ptr)))))

    (define (rear-delete!)
      (cond
        ((empty?) (error "DELETE! вызвана с пустой очередью"))
        (else
          (begin
            ;(cddr )
            (set-rear-ptr! (cddr rear-ptr))))))


    (define (print)
      (define (iter item)
        (if (eq? item rear-ptr)
          (begin
            (cond ((not (null? item)) (display (car item))))
            (display ")")
            (newline))
          (begin
            (display (car item))
            (display " ")
            (iter (cadr item)))))

      (display "(")
      (iter front-ptr))


    (define (dispatch m)
      (cond
        ((eq? 'empty? m) empty?)
        ((eq? 'front m) front)
        ((eq? 'rear m) rear)
        ((eq? 'front-insert! m) front-insert!)
        ((eq? 'rear-insert! m) rear-insert!)
        ((eq? 'front-delete! m) front-delete!)
        ((eq? 'rear-delete! m) rear-delete!)
        ((eq? 'print m) print)
        (else (error "Unknown operation " m))))

    dispatch))

(define q (make-dequeue))

((q 'print))
(puts ((q 'empty?)))

((q 'rear-insert!) 3)
((q 'rear-insert!) 4)
((q 'rear-insert!) 5)
((q 'front-insert!) 2)
((q 'front-insert!) 1)
((q 'print))
(puts ((q 'empty?)))

((q 'rear-delete!))
((q 'rear-delete!))
((q 'front-delete!))
((q 'front-delete!))
((q 'print))

((q 'front-insert!) 1)
((q 'rear-insert!) 5)
((q 'print))
