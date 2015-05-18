#lang racket

(require json
         racket/runtime-path
         rackunit)

(define-runtime-path here ".")

(define (model-file->jsexpr p)
  (bytes->jsexpr
   (second (regexp-match #px#"^\\w+\\s+=\\s+(.*)" (file->bytes p)))))


(check-equal?
 (model-file->jsexpr "/tmp/atotc-3-tree-hash.js")
 (model-file->jsexpr (build-path here "atotc-3-tree-hash.js")))