#lang setup/infotab

(define collection 'multi)

(define deps
  (list "base"
        "rackunit-lib"
        "scribble-lib"
        "typed-racket-lib"
        "typed-racket-more"
        "web-server-lib"))

(define build-deps
  (list "rackunit-lib"
        "racket-doc"))

