#lang racket

(require racket/runtime-path
         json
         rackunit)

;; this maps a password back to the hex string that generated it. This
;; code is a bit rough--it depends on the existence of files (a serialized
;; model) that isn't part of the distribution. I'm disabling testing:
(module* test racket/base)

(define-runtime-path here ".")

(define (file->jsexpr path)
  (bytes->jsexpr
   (call-with-input-file path
     (lambda (port)
       (second (regexp-match #px#"^[a-zA-Z]+\\s*=\\s*(.*);\\s*$" port))))))

(define (jstree->cons-tree t)
  (cond [(hash? t)
         (cons (jstree->cons-tree (hash-ref t 'a))
               (jstree->cons-tree (hash-ref t 'b)))]
        [else t]))

(define (jshash->hash-of-cons-trees ht)
  (for/hash ([(k v) (in-hash ht)])
    (values (symbol->string k) (jstree->cons-tree v))))

(define seed-tree
  (jstree->cons-tree
   (file->jsexpr (build-path here "atotc-2-seed-tree.js"))))

(define tree-jsexpr (file->jsexpr (build-path here "atotc-2-tree-hash.js")))

(define tree-hash
  (jshash->hash-of-cons-trees tree-jsexpr))


;; given a cons-tree and a leaf and a reversed path,
;; return a complete reversed path if the leaf is in
;; the tree, #f otherwise
(define (find-leaf t l path)
  (cond [(cons? t) (or (find-leaf (car t) l (cons #t path))
                       (find-leaf (cdr t) l (cons #f path)))]
        [else (cond [(equal? t l) path]
                    [else #f])]))

(check-equal? (find-leaf '(a . b) 'a '()) '(#t))
(check-equal? (find-leaf '((a . c) . b) 'c '()) '(#f #t))

(define (text->charbits text)
  (define spacetext (string-append " " text))
  (cons
   (reverse (find-leaf seed-tree (substring spacetext 0 2) '()))
   (for/list ([i (in-range 2 (string-length text))])
     (define state (substring spacetext (- i 2) i))
     (define leaf (substring spacetext i (add1 i)))
     (define maybe-path
       (find-leaf (hash-ref tree-hash state)
                  leaf
                  '()))
     (match maybe-path
       [#f (error 'text->charbits
                  "no path to leaf ~v found from state ~v"
                  leaf state)]
       [(? list? l) (reverse l)]))))
(define bitlists (text->charbits "a"))
bitlists
(define allbits (apply append bitlists))
(length allbits)
#;allbits

