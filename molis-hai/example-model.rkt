#lang typed/racket/base

(require "char-model.rkt"
         racket/runtime-path
         (only-in racket/file file->string))

(define-runtime-path here ".")

(provide atotc-model)

(define atotc-path (build-path here "a-tale-of-two-cities.txt"))

(define atotc-model (build-char-model 2 (file->string atotc-path)))

