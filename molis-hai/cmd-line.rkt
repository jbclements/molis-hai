#lang racket

(require racket/runtime-path
         "char-model.rkt"
         "use-model.rkt")

(define-runtime-path here ".")

(define atotc-path (build-path here "a-tale-of-two-cities.txt"))

(define model (build-char-model 2 (file->string atotc-path)))

(define ENTROPY-BITS 56)

(display (substring (generate-char-pwd model ENTROPY-BITS) 1))

