(define (puts x)
  (display x)
  (newline))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond
        ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
             (if record
                (cdr record)
                #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Неизвестная операция -- TABLE" m))))

    dispatch))

(define (same-key? table-key input-key)
  (< (abs (- table-key input-key)) 5))

(define operation-table (make-table same-key?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(put 1 1 'value)
(put 1 10 'value-10)
(put 20 3 'value-20)

(puts (get 2 3))
