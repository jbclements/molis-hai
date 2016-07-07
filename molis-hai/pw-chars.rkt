#lang racket

;; if you just want dense passwords, you can use
;; raco molis-hai -t pw-chars.txt -o 0 -n 1 -b 56
;;
;; ... but if you use an order greater than zero, you're not going to
;; like the result...

(module+ main
  (define pw-chars
    (string-append
     (list->string
      (append
       (for/list ([i (in-range (char->integer #\a) (add1 (char->integer #\z)))])
         (integer->char i))
       (for/list ([i (in-range (char->integer #\A) (add1 (char->integer #\Z)))])
         (integer->char i))
       (for/list ([i (in-range (char->integer #\0) (add1 (char->integer #\9)))])
         (integer->char i))))
     "!@#$%^&*()[]-{}|=+/?_,.<>;:"))
  
  (with-output-to-file "pw-chars.txt"
    (λ () (display pw-chars)))
  
  )