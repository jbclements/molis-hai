#lang racket

(require (planet neil/html-parsing)
         racket/runtime-path)

(define-runtime-path here ".")

(define sxml
  (call-with-input-file (build-path here "crime-and-punishment.html")
    html->xexp))