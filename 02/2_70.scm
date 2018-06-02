(define (puts x)
  (display x)
  (newline))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
        (choose-branch (car bits) current-branch)))

        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))

  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (error "плохой бит -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set
        (make-leaf (car pair) (cadr pair))
        (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
    '()
    (append
      (encode-symbol (car message) tree)
      (encode (cdr message) tree))))

(define (contains? item l)
  (any (lambda (x) (eq? item x)) l))

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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (define (iter res)
    (if (< (length res) 3)
      res
      (iter
        (adjoin-set
          (make-code-tree (car res) (cadr res))
          (cddr res)))))

  (iter set))

(define tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(define text '(
  (Get a job)
  (Sha na na na na na na na na)
  (Get a job)
  (Sha na na na na na na na na)
  (Wah yip yip yip yip yip yip yip yip yip)
  (Sha boom)))

(puts (fold-right + 0 (map length text)))

(define encoded (map (lambda (l) (encode l tree)) text))

(puts (fold-right + 0 (map length encoded)))

(puts (map (lambda (l) (decode l tree)) encoded))


