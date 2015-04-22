#lang typed/racket

;; copyright 2015 John Clements <clements@racket-lang.org>

(require typed/rackunit)

(define bytesize 16)
(define logbytesize 4)

(provide (struct-out Leaf)
         (struct-out Branch)
         MyTree
         clump)

;; this file defines functions to create huffman trees. 

;; count : loc -> locc
(: loc->countvec ((Listof Byte) -> (Vectorof Natural)))
(define (loc->countvec loc)
  (: vec (Vectorof Natural))
  (define vec (make-vector bytesize 0))
  (for ([elt (in-list loc)])
    (vector-set! vec elt (+ (vector-ref vec elt) 1)))
  vec)

(check-equal? (loc->countvec (ann '(1 2 11 2 14) (Listof Byte)))
      (ann (vector 0 1 2 0 0 0 0 0 0 0 0 1 0 0 1 0)
           (Vectorof Natural)))
;;; trees

(define-struct (T) Leaf ([wt : Natural] [n : T]) #:transparent)
(define-struct (T) Branch ([wt : Natural] [l : (MyTree T)] [r : (MyTree T)]) #:transparent)

(define-type (MyTree T) (U (Leaf T) (Branch T)))

;; return the weight of a leaf or branch
(: nodeweight (All (T) ((MyTree T) -> Natural)))
(define (nodeweight tree)
  (cond [(Leaf? tree) (Leaf-wt tree)]
        [else (Branch-wt tree)]))


(: countvec->treelist ((Vectorof Natural) -> (Listof (Leaf Byte))))
(define (countvec->treelist countvec)
  (for/list ([posn : Index (ann (in-range (ann 256 Index))
                                (Sequenceof Index))]
             [count (in-vector countvec)])
    (Leaf count (assert posn byte?))))

(: ensure-byte (Natural -> Byte))
(define (ensure-byte b)
  (cond [(byte? b) b]
        [else (error 'not-a-byte)]))

(check-equal? (countvec->treelist (vector 0 1 2 0 0 0 0 0 0 0 0 1 0 0 1 0))
              (list (Leaf 0 0)
                    (Leaf 1 1)
                    (Leaf 2 2)
                    (Leaf 0 3)
                    (Leaf 0 4)
                    (Leaf 0 5)
                    (Leaf 0 6)
                    (Leaf 0 7)
                    (Leaf 0 8)
                    (Leaf 0 9)
                    (Leaf 0 10)
                    (Leaf 1 11)
                    (Leaf 0 12)
                    (Leaf 0 13)
                    (Leaf 1 14)
                    (Leaf 0 15)))


;; keep clumping until we get one tree
(: clump (All (T) ((Listof (MyTree T)) -> (MyTree T))))
(define (clump treelist)
  (if (not (null? (cdr treelist)))
      (clump (one-clump treelist))
      (car treelist)))

;; perform one huffman clumping
(: one-clump (All (T) ((Listof (MyTree T)) -> (Listof (MyTree T)))))
(define (one-clump treelist)
  (let* ([l (sort treelist (lambda ([a : (MyTree T)] 
                                    [b : (MyTree T)])
                             (< (nodeweight a) (nodeweight b))))]
         [a (car l)]
         [b (cadr l)])
    (cons (Branch (+ (nodeweight a) (nodeweight b)) a b) (cddr l))))

(check-equal? (one-clump (list (Branch 13 (Leaf 2 14) (Leaf 11 1)) (Leaf 2 2) (Branch 0 (Leaf 0 3) (Leaf 0 4))))
              (list (Branch 2 (Branch 0 (Leaf 0 3) (Leaf 0 4))
                            (Leaf 2 2))
                    (Branch 13 (Leaf 2 14) (Leaf 11 1))))

(check-equal? (clump (list (Branch 13 (Leaf 2 14) (Leaf 11 1)) (Leaf 2 2) (Branch 0 (Leaf 0 3) (Leaf 0 4))))
              (Branch 15
                      (Branch 2 (Branch 0 (Leaf 0 3) (Leaf 0 4))
                              (Leaf 2 2))
                      (Branch 13 (Leaf 2 14) (Leaf 11 1))))


;; INVERSION


;; an encode-vec is a vector of length bytesize containing len & bits
(define-type EncodeVec (Vectorof (U False (Vector Natural Natural))))


(: invert ((MyTree Byte) -> EncodeVec))
(define (invert tree)
  (: outvec EncodeVec)
  (define outvec (make-vector bytesize #f))
  (let loop ([t tree] [path-length : Natural 0] [path : Natural 0])
    (cond [(Leaf? t)
           (vector-set! outvec (Leaf-n t) 
                        (ann (vector path-length path)
                             (Vector Natural Natural)))]
          [else
           (loop (Branch-l t) (+ path-length 1) (* 2 path))
           (loop (Branch-r t) (+ path-length 1) (+ 1 (* 2 path)))]))
  outvec)

(: test-table EncodeVec)
(define test-table (vector (vector 3 1)
                           (vector 2 3)
                           (vector 2 1)
                           (vector 3 0)
                           #f
                           #f
                           #f
                           #f
                           #f
                           #f
                           #f
                           #f
                           #f
                           #f
                           (vector 2 2)
                           #f))

(define test-tree
  (Branch 15
          (Branch 2 (Branch 0 (Leaf 0 3) (Leaf 0 0))
                  (Leaf 2 2))
          (Branch 13 (Leaf 2 14) (Leaf 11 1))))

(check-equal? (invert test-tree)
      test-table)

(: shift-and-add (Natural Natural Natural -> Natural))
(define (shift-and-add num new-num new-len)
  (+ new-num (* num (expt 2 new-len))))

(check-equal? (shift-and-add 12 3 3) ;; 1100011
      (+ 64 32 3))

;; invariant : spill-len < logbytesize

(: encode (EncodeVec (Listof Natural) -> (Listof Byte)))
(define (encode table loc)
  (let loop ([loc loc] [spill : Natural 0] [spill-len 0])
    (cond [(null? loc) 
           (list 
            (assert (shift-and-add spill 0 
                                   (assert (- logbytesize spill-len)
                                           exact-nonnegative-integer?))
                    byte?))]
          [else (define tl (vector-ref table (car loc)))
                (cond [(false? tl) (error 'oh-dear)]
                      [else
                       (let* ([next (shift-and-add spill (vector-ref tl 1) (vector-ref tl 0))]
                              [new-len (+ spill-len (vector-ref tl 0))])
                         (if (> new-len logbytesize)                 
                             (let* ([new-spill-len (assert (- new-len logbytesize)
                                                           exact-nonnegative-integer?)]
                                    [new-spill (modulo next (expt 2 new-spill-len))])
                               (cons (assert
                                      (/ (- next new-spill) (expt 2 new-spill-len))
                                      byte?)
                                     (loop (cdr loc) new-spill new-spill-len)))
                             (loop (cdr loc) next new-len)))])])))

(check-equal? (encode test-table '(2 1 3 14 2 0))
      ;; 2  1  3   14 2  0
      ;; 01 11 000 10 01 001
      ;; 0111 0001 0010 01 00
      (list 7 1 2 4))

(: decode ((MyTree Byte) (Listof Byte) -> (Listof Byte)))
(define (decode tree loc)
  (let loop ([t tree] 
             [cur-bit : Exact-Rational (/ bytesize 2)]
             [carloc (car loc)]
             [cdrloc (cdr loc)])
    (if (< cur-bit 1)
        (loop t (/ bytesize 2) (car cdrloc) (cdr cdrloc))
        (cond [(Leaf? t)
               (define n (Leaf-n t))
               (if (= n 0)
                   empty
                   (cons n (loop tree cur-bit carloc cdrloc)))]
              [else
               (define l (Branch-l t))
               (define r (Branch-r t))
               (if (>= carloc cur-bit)
                   (loop r (/ cur-bit 2) (- carloc (assert cur-bit byte?)) cdrloc)
                   (loop l (/ cur-bit 2) carloc cdrloc))]))))


(check-equal? (decode test-tree (list 7 1 2 4))
      (list 2 1 3 14 2))

;; given a list of bytes, compute the huffman tree
(: compute-tree ((Listof Byte) -> (MyTree Byte)))
(define (compute-tree loc)
  (clump (countvec->treelist (loc->countvec loc))))


(set! bytesize 256)
(set! logbytesize 8)

(define (g)
  (define a (append (bytes->list #"my dog is named fred.  He's my favourite companion.") (list 0)))
  (length a)
  (define t (compute-tree a))
  (define b (encode (invert t) a))
  (length b)
  (list->bytes (decode t b)))
