(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (encode-symbol symbol tree)
  (define (iter res current-tree)
    (if (leaf? current-tree)
      res
      (let (
        (left (left-branch current-tree))
        (right (right-branch current-tree)))

        (cond
          ((contains? symbol (symbols left)) (iter (append res (list 0)) left))
          ((contains? symbol (symbols right)) (iter (append res (list 1)) right))
          (else (error "unsupported symbol" symbol))))))

  (iter '() tree))

;O(n) for most frequent ~ 1
;O(n) for most rare ~ log(n) * n * n
