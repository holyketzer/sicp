(define (get-record name department)
  (if (null? department)
    '()
    (let (
      (record (fetch-next-record department)))

      (if (= name (fetch-name record))
        record
        (get-record name (fetch-rest-department department))))))

(define (get-salary name department)
  (let (
    (record (get-record name department)))

    (if (null? record)
      '()
      (fetch-salary record))))

(define (find-employee-record name departments)
  (let (
    (record (get-record name (car departments))))

    (if (null? record)
      (find-employee-record name (car departments))
      record)))
