(define (puts x)
  (display x)
  (newline))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key tree)
  (if (null? tree)
    false
    (let ((item (entry tree)))
      (let ((item-key (key item)))
        (cond
          ((equal? given-key key) item)
          ((< given-key key) (lookup given-key (left-branch tree)))
          ((> given-key key) (lookup given-key (right-branch tree))))))))
