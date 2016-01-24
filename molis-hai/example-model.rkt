#lang typed/racket/base

(require "char-model.rkt"
         racket/runtime-path
         (only-in racket/file file->string))

(define-runtime-path here ".")

(provide atotc-model
         base-model)


;; does this string start with a space?
(: starts-with-space? (String -> Boolean))
(define (starts-with-space? str)
  (equal? (string-ref str 0) #\space))

(define atotc-path (build-path here "a-tale-of-two-cities.txt"))

(define atotc-model (build-char-model 2 (file->string atotc-path)
                                      starts-with-space?))

;; a model that just generates passwords from a large set of chars

(define BASE-CHARS
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_(){}[]<>/+-!@#$%^&*,.?=")

(define base-model (build-char-model 0 BASE-CHARS (λ (s) #t)))


