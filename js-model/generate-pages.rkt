#lang racket

(require web-server/templates
         racket/runtime-path
         xml)

(define-runtime-path here ".")

;; plug vars into the index template
(define (make-pwgen-page order tree-hash-path seed-tree-path)
  (include-template "pwgen-template.html"))

;; add quotes
(define (with-quotes str)
  (~a "\""str"\""))

(define (build-page order)
  (display-to-file
   (make-pwgen-page order
                    (with-quotes (~a "atotc-"order"-tree-hash.js"))
                    (with-quotes (~a "atotc-"order"-seed-tree.js")))
   (build-path here (~a "pwgen-"order".html"))
   #:exists 'truncate))

(for ([i (in-range 1 4)])
  (build-page i))



