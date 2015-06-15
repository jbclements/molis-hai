#lang info

(define name "Molis Hai")

(define compile-omit-paths '("experimental"))
(define scribblings '(("molis-hai.scrbl" () (tool))))

(define raco-commands
  (list
   (list "molis-hai"
         "cmd-line.rkt"
         "generate secure passwords using a source text"
         #f)))