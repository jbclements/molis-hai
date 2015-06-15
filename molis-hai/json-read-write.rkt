#lang racket

;; functions to write models to disk and read them back again.

(require json
         ;; merge this into this file...
         "huffman-convert.rkt"
         "shared-types.rkt")

(provide (contract-out [model-write (-> Model? path-string? void?)]
                       #;[model-read (-> path-string? Model?)]))


;; given a model and a path, write the model to the path using a JS-friendly
;; format.
(define (model-write m path)
  (call-with-output-file path
    (lambda (port)
      (write-jsexpr "SeedModel" (huffman-tree->jsexpr str->str (Model-seed-chooser m))
                    port)
      (write-jsexpr "TextModel" (trees-hash->jsexpr (Model-trans m)) port))
    #:exists 'truncate))

;; write a jsexpr to a js file so it can be loaded in javascript
(define (write-jsexpr top-level-varname jsexpr port)
  (fprintf port "~a = \n" top-level-varname)
  (write-json jsexpr port)
  (display ";\n" port))

;; convert a transition map to a jsexpr
(define (trees-hash->jsexpr trees-hash)
  (for/hash ([(k v) (in-hash trees-hash)])
    (values (string->symbol k)
            (huffman-tree->jsexpr char->str v))))

(define (str->str s) s)
(define (char->str ch) (string ch))

;; given a path, read a model from it
#;(define (model-read path)
  (call-with))

(module+ test

  #;(define test-model
    (Model (Branch 0 (Branch 0 (Leaf "a") (Leaf "b")) (Leaf "c"))))

  )

