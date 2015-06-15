#lang racket

(require racket/runtime-path
         "build-model.rkt"
         "use-model.rkt")

(define-runtime-path here ".")

(define atotc-path (build-path here "a-tale-of-two-cities.txt"))

(define model (build-model 2 atotc-path))

(define ENTROPY-BITS 56)

(substring (generate-pwd model ENTROPY-BITS) 1)

