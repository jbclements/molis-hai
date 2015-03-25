#lang racket/base
; From http://con.racket-lang.org/pr-slides.pdf
; by Prabhakar Ragde
(require scribble/html-properties
         scribble/base
         scribble/core)

(provide setup-math math-in math-disp $ $$)

(define mathjax-source
  "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  ; "http://c328740.r40.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=default"
  ;"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-HTML"
  )

(define setup-math
  (paragraph 
   (style 
    #f (list (alt-tag "script")
             (attributes `((type . "text/javascript")
                           (src . ,mathjax-source )))))
   '()))

(define (mymath start end . strs)
  (make-element (make-style "relax" '(exact-chars)) `(,start ,@strs ,end)))

(define (math-in . strs) 
  (apply mymath "\\(" "\\)" strs))

(define (math-disp . strs)
  (apply mymath "\\[" "\\]" strs))

(define $ math-in)
(define $$ math-disp)


