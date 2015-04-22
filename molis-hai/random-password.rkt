#lang racket

(random-seed (current-seconds))
(define PW-LENGTH 9 #;56)
(define num-choices 8)

(define CHARS 
  (string->list #;"01" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!^-=+[]@#$%&*()"))


(define (random-char)
  (list-ref CHARS (random (length CHARS))))

(define (random-password)
  (list->string (build-list PW-LENGTH (lambda (x) (random-char)))))

(provide random-password)
(for ([p (build-list num-choices (lambda (x) (random-password)))])
  (display p)
  (newline))

(printf "bits of entropy: ~v\n" (* (/ (log (length CHARS)) (log 2)) PW-LENGTH))

(length CHARS)

