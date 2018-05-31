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

(define (tree->list tree)
  (if (null? tree)
    '()
    (append
      (tree->list (left-branch tree))
      (cons
        (entry tree)
        (tree->list (right-branch tree))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let (
      (left-size (quotient (- n 1) 2)))

      (let (
        (left-result (partial-tree elts left-size)))

        (let (
          (left-tree (car left-result))
          (non-left-elts (cdr left-result))
          (right-size (- n (+ left-size 1))))

          (let (
            (this-entry (car non-left-elts))
            (right-result (partial-tree (cdr non-left-elts) right-size)))

          (let (
            (right-tree (car right-result))
            (remaining-elts (cdr right-result)))

            (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

(define (union-set t1 t2)
  (define (merge-lists res l1 l2)
    (cond
      ((null? l1) (append res l2))
      ((null? l2) (append res l1))
      (else
        (let (
          (i1 (car l1))
          (i2 (car l2)))

          (cond
            ((= i1 i2) (merge-lists (append res (list i1)) (cdr l1) (cdr l2)))
            ((< i1 i2) (merge-lists (append res (list i1)) (cdr l1) l2))
            (else (merge-lists (append res (list i2)) l1 (cdr l2))))))))

  (let (
    (l1 (tree->list t1))
    (l2 (tree->list t2)))

    (list->tree (merge-lists '() l1 l2))))

(define tree1 (list->tree '(1 2 3 4 5)))
(define tree2 (list->tree '(3 4 5 6 7)))

(puts tree1)
(puts tree2)

(puts (tree->list (union-set tree1 tree2)))

