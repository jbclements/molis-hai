#lang typed/racket

(provide make-table
         table-row-count
         in-table-column
         table-ref
         table-ref1)

;; a table is (table (listof symbol) (listof vector))
;; containing a list of column names and a list of vectors
;; of the specified length
(struct Table ([name->idx : (HashTable Symbol Index)]
               [idx->name : (HashTable Index Symbol)]
               [data : (Listof (Vectorof Any))]))

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
(: make-table ((Sequenceof Symbol) (Sequenceof (Sequenceof Any)) -> Table))
(define (make-table nameseq dataseq)
  (define names (sequence->list nameseq))
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
  (Table n2i i2n data))

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
  )