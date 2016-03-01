#lang typed/racket

(provide make-table
         table-row-count
         in-table-column
         table-ref
         table-ref1
         select-where
         sub-table)

;; a table is (table (listof symbol) (listof vector))
;; containing a list of column names and a list of vectors
;; of the specified length
(struct Table ([name->idx : (HashTable Symbol Index)]
               [idx->name : (HashTable Index Symbol)]
               ;; kept just to allow sub-tables:
               [names     : (Listof Symbol)]
               [data : (Listof (Vectorof Any))]
               [indexes : (Boxof (Listof Table-Index))]))

;; a table-index is a hash table from tuples of values from certain columns
;; to the rows that match those values. This is great as long as you don't
;; allow non-equality constraints.
(struct Table-Index ([columns : (Setof Symbol)]
                     [index : (HashTable (Vectorof Any) (Vectorof (Vectorof Any)))])
  #:transparent)

;; are two tables equal?
(: table-equal? (Table Table -> Boolean))
(define (table-equal? t1 t2)
  (and (equal? (Table-names t1)
               (Table-names t2))
       (equal? (Table-data t1)
               (Table-data t2))))

;; construct a two-way lookup table between the list of names
;; and the corresponding integers
(: names->lookup-tables ((Listof Symbol) -> (Values (HashTable Symbol Index)
                                                    (HashTable Index Symbol))))
(define (names->lookup-tables names)
  (values
   (for/hash : (HashTable Symbol Index)
     ([n (in-list names)]
      [i (in-naturals)])
     (cond [(index? i) (values n i)]
           [else (error 'names->lookup-tables
                        "way way way too many names")]))
   (for/hash : (HashTable Index Symbol)
     ([n (in-list names)]
      [i (in-naturals)])
     (cond [(index? i) (values i n)]
           [else (error 'names->lookup-tables
                        "way way way too many names")]))))

;; given a table, return the number of rows it has
(: table-row-count (Table -> Index))
(define (table-row-count table)
  (length (Table-data table)))

;; given a sequence of names of length 'n' and a sequence of (sequences of length 'n'),
;; return a table. Columns are labeled by the names.
(: make-table ((Listof Symbol) (Sequenceof (Sequenceof Any)) -> Table))
(define (make-table names dataseq)
  (define name-count (length names))
  (: data (Listof (Vectorof Any)))
  (define data (map (ann sequence->vector
                         ((Sequenceof Any) -> (Vectorof Any)))
                    (sequence->list dataseq)))
  (when (check-duplicates names)
    (raise-argument-error 'make-table
                          "unique names" 0 names data))
  (unless (andmap (λ ([row : (Vectorof Any)])
                    (= name-count (vector-length row))) data)
    (raise-argument-error 'make-table
                          (format "rows of length ~v" name-count)
                          data))
  (define-values (n2i i2n) (names->lookup-tables names))
  (Table n2i i2n names data (box '())))

;; imperatively add an index on the given fields to the table
(: add-index! (Table (Listof Symbol) -> Void))
(define (add-index! table cols)
  (: selected-idxs (Vectorof Index))
  (define selected-idxs (for/vector : (Vectorof Index)
                          ([col (in-list cols)])
                          (find-idx table col)))
  (define new-index
    (for/fold ([ht : (HashTable (Vectorof Any) (Listof (Vectorof Any))) (hash)])
              ([row (in-list (Table-data table))])
      (define key (for/vector ([idx (in-vector selected-idxs)])
                    (vector-ref row idx)))
      (hash-set ht key (cons row (hash-ref ht key (λ () '()))))))
  (define vectorized-index
    (for/hash : (HashTable (Vectorof Any)
                           (Vectorof (Vectorof Any)))
      ([(key value) (in-hash new-index)])
      (values key (list->vector value))))
  (set-box! (Table-indexes table) (cons (Table-Index (list->set cols) vectorized-index)
                                        (unbox (Table-indexes table)))))

;; convert a sequence to a vector
(: sequence->vector (All (T) ((Sequenceof T) -> (Vectorof T))))
(define (sequence->vector s)
  (list->vector (sequence->list s)))

;; given a table and a column name, return
;; a sequence (currently just a list) of the values in
;; the table's column
(: in-table-column (Table Symbol -> (Sequenceof Any)))
(define (in-table-column table name)
  (define col-idx (find-idx table name))
  (remove-duplicates
   (map (λ ([row : (Vectorof Any)]) (vector-ref row col-idx))
        (Table-data table))))

;; given a table and a source and destination column and a value,
;; return the list of values in the destination column for rows
;; where the value in the source column is equal to the given value.
(: table-ref (Table Symbol Symbol Any -> (Listof Any)))
(define (table-ref table from-col to-col val)
  (define from-idx (find-idx table from-col))
  (define to-idx (find-idx table to-col))
  ;; when to cache this search and create an index?
  (for/list ([row (in-list (Table-data table))]
             #:when (equal? (vector-ref row from-idx) val))
    (vector-ref row to-idx)))

;; given a table and a source and destination column and a value,
;; return the value in the destination column for the unique row
;; where the value in the source column is equal to the given value.
(: table-ref1 (Table Symbol Symbol Any -> Any))
(define (table-ref1 table from-col to-col val)
  (match (table-ref table from-col to-col val)
    [(list r) r]
    [other (raise-argument-error 'table-ref1
                                 "unique key" 3
                                 table from-col to-col val)]))

(: find-idx (Table Symbol -> Index))
(define (find-idx table col-name)
  (match (hash-ref (Table-name->idx table) col-name)
    [#f (raise-argument-error 'find-idx "known field name" 1 table col-name)]
    [idx idx]))

;; looks like we need something like a "select". given a table
;; and a sequence of columns and a sequence of constraints, return
;; a sequence of vectors containing the requested columns from
;; the rows matching the constraints. For now, a constraint is
;; simply a list of length two containing a column name and a
;; value, and represents an equality constraint
(define-type Constraint (List Symbol Any))
(: select-where (Table (Listof Symbol) (Listof Constraint) -> (Sequenceof (Vectorof Any))))
(define (select-where table cols constraints)
  (: idx-constraints (Listof (List Index Any)))
  (define idx-constraints (for/list ([constraint : Constraint constraints])
                            (list (find-idx table (first constraint))
                                  (second constraint))))
  (: selected-idxs (Vectorof Index))
  (define selected-idxs (for/vector : (Vectorof Index)
                          ([col cols])
                          (find-idx table col)))
  (: pred ((Vectorof Any) -> Boolean))
  (define (pred row)
    (for/and ([c (in-list idx-constraints)])
      (equal? (vector-ref row (first c)) (second c))))
  (for/list : (Listof (Vectorof Any))
    ([row (in-list (Table-data table))]
     #:when (pred row))
    (for/vector ([idx selected-idxs])
      (vector-ref row idx))))

;; given a table and a list of constraints, compute a sub-table
;; containing only the rows satisfying those constraints.
(: sub-table (Table (Listof Constraint) -> Table))
(define (sub-table table constraints)
  (: idx-constraints (Listof (List Index Any)))
  (define idx-constraints (for/list ([constraint : Constraint constraints])
                            (list (find-idx table (first constraint))
                                  (second constraint))))
  (: pred ((Vectorof Any) -> Boolean))
  (define (pred row)
    (for/and ([c (in-list idx-constraints)])
      (equal? (vector-ref row (first c)) (second c))))
  (define kept-rows
   (for/list : (Listof (Vectorof Any))
     ([row (in-list (Table-data table))]
      #:when (pred row))
     row))
  (make-table (Table-names table)
              kept-rows))

(module+ test
  (require typed/rackunit)

  (define my-table (make-table '(a b c)
                               '(#("a" 1 "d")
                                 #("a" 2 "d")
                                 #("a" 3 "e")
                                 #("b" 4 "abch"))))

  (check-equal?
   (list->set (sequence->list (in-table-column my-table 'c)))
   (set "d" "e" "abch"))

  (check-equal? (list->set (table-ref my-table 'a 'c "a"))
                (list->set (list "d" "e")))

  (check-equal? (table-ref1 my-table 'b 'c 4) "abch")

  (check-equal? (table-row-count my-table) 4)

  (check-equal? (sequence->list (select-where my-table '(b c) '((a "a") (c "d"))))
                (list (vector 1 "d")
                      (vector 2 "d")))

  (check table-equal?
         (sub-table my-table '((c "d")))
         (make-table '(a b c)
                     '(#("a" 1 "d")
                       #("a" 2 "d"))))

  ;; WARNING: TEST OF MUTABLE STATE
  (check-equal? (unbox (Table-indexes my-table)) '())
  (add-index! my-table '(c a))
  (check-equal? (unbox
                 (Table-indexes
                  my-table))
                (cons (Table-Index
                       '(c a)
                       (ann
                        ((ann make-immutable-hash
                              ((Listof (Pairof (Vectorof Any)
                                               (Vectorof (Vectorof Any))))
                               -> (HashTable (Vectorof Any)
                                             (Vectorof (Vectorof Any)))))
                         '((#("d" "a") . #(#("a" 2 "d") #("a" 1 "d")))
                           (#("e" "a") . #(#("a" 3 "e")))
                           (#("abch" "b") . #(#("b" 4 "abch")))))
                        (HashTable (Vectorof Any)
                                   (Vectorof (Vectorof Any)))))
                      empty))
  )