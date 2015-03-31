#lang racket

(require data/enumerate
         data/enumerate/lib
         rackunit)


;; given a tree-table, a rotation function, an initial state, and
;; a bitcount, produce a list of leaves chosen.
(define (go tree-table rotate init-state bitcount)
  (define new-huff-tree (hash-ref tree-table init-state))
  (cons-tree-enum/top new-huff-tree bitcount
                      (make-kont tree-table rotate init-state)))

;; the kont continuation accepts a leaf and a bitcount
;; and produces an enumeration of size 2^bitcount. In our
;; usage, it will already be closed over the current
;; markov state.
(define (make-kont tree-table rotate markov-state)
  (lambda (leaf bitcount)
    (define new-state (rotate markov-state leaf))
    (define new-huff-tree (hash-ref tree-table new-state))
    (cons-tree-enum/top new-huff-tree bitcount
                        (make-kont tree-table rotate new-state))))


;; this shortens pwds by one char when you run out
;; of bits right at the boundary between tree traversals:
(define (cons-tree-enum/top tree bitcount kont)
  (cond [(= bitcount 0) (fin/e empty)]
        [else (cons-tree-enumeration tree bitcount kont)]))

(define (cons-tree-enumeration tree bitcount kont)
  (thunk/e
   (lambda ()
     (cond [(pair? tree)
            (cond [(= bitcount 0)
                   ;; uh oh... we're part way through the tree traversal
                   (cons/e (fin/e (bailout (list tree))) (fin/e empty))]
                  [(or/e (cons-tree-enumeration (car tree) (sub1 bitcount) kont)
                         (cons-tree-enumeration (cdr tree) (sub1 bitcount) kont))])]
           [else (cons/e (fin/e tree)
                         (kont tree bitcount))]))
   #:count (expt 2 bitcount)))

;; find the most likely (a.k.a. shallowest) leaf in a list of trees.
;; break ties to the left.
(define (bailout trees)
  (define maybe-done (ormap (lambda (tree) (and (not (pair? tree)) tree)) trees))
  (cond [maybe-done maybe-done]
        [else (define flatten1
                (for/list ([t trees])
                  (list (car t) (cdr t))))
              (bailout (apply append flatten1))]))

(check-equal? (bailout '((a . b) (d . e) f (g . h))) 'f)
(check-equal? (bailout '(((a . i) . (b . i)) (d . (e . i)) ((g . i) . h))) 'd)


(define test-table (hash (list 'a 'a) 'b
                         (list 'a 'b) '((a . b) . c)
                         (list 'b 'a) '(a . b)
                         (list 'b 'b) 'c
                         (list 'b 'c) '((c . b) . a)
                         (list 'c 'a) 'b
                         (list 'c 'b) '((a . b) . c)
                         (list 'c 'c) '(b . a)
                         ))

(define (test-rotate state leaf)
  (append (cdr state) (list leaf)))

(define test-enumeration (go test-table test-rotate '(a b) 3))

(check-equal?
 (list->set (enum->list test-enumeration))
 (list->set
  '((a a) ;lll
    (a b) ;llr
    (b c c) ;lrl
    (b c a) ;lrr
    (c c) ;rll
    (c b);rlr
    (c a b a) ;rrl
    (c a b c) ; rrr
    )))

(to-nat test-enumeration '(c a b c))
(to-nat test-enumeration '(c a b c))

;; but now, what about this one:

(define bad-test-table
  (hash (list 'a 'a) 'b
        (list 'a 'b) '((a . b) . c)
        (list 'b 'a) '(a . b)
        (list 'b 'b) 'c
        (list 'b 'c) '((c . b) . c)
        (list 'c 'a) 'b
        (list 'c 'b) '((a . b) . c)
        (list 'c 'c) '(b . a)
        ))

(define bad-test-enumeration (go bad-test-table test-rotate '(a b) 3))

;; contains a duplicate!
(check-equal? (length (remove-duplicates (enum->list bad-test-enumeration)))
              (enum-count bad-test-enumeration))
