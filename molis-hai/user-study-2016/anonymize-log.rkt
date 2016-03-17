#lang racket

;; this file contains the code used to anonymize the logs and
;; write them to a '.tsv' file. It uses the salt string appearing
;; in /tmp/salt.txt.

#;(
   (require db)
   ;; experimenting with sqlite... this code is currently unused
(define-runtime-path db-path "/tmp/log.db")

(define conn
  (sqlite3-connect #:database 'memory #;db-path
                   #:mode 'read/write))

(query-exec conn
            "CREATE TABLE sessions (server_ts INTEGER, userid TEXT, key TEXT \
PRIMARY KEY, training_str TEXT);")

(query-exec conn
            "CREATE TABLE keystrokes (server_ts INTEGER, key TEXT, \
timestamp INTEGER, boxnum INTEGER, content TEXT, FOREIGN KEY (key) REFERENCES sessions);"))

;server-timestamp key timestamp boxnum content

;server-timestamp userid key training-str




(require racket/runtime-path
         sha
         net/base64
         threading
         rackbot/tai64n
         srfi/19
         racket/block
         racket/struct)


;; these users are just a consequence of testing... ignore them.
(define bogus-users
  (list "consenter"
        "nonconsenter"
        "guest"
        "clements"))

;; likewise
(define bogus-session-keys
  (list "7728koh"))

;; this string occurs once and appears to be some kind of auto-paste:
(define bogus-boxentries
  (list "(string->symbol (randomTerm maxDepth))"))

;; represents a start-of-session log line
(struct session-start [server-timestamp userid key training-str] #:transparent)
(define session-start-col-names '(server-timestamp userid key training-str))
;; represents a keystroke log line
(struct session-data [server-timestamp key timestamp boxnum content] #:transparent)
(define session-data-col-names '(server-timestamp key timestamp boxnum content))

(define-runtime-path here ".")

(define USERS-LIST-FILE (build-path here "users.rktd"))

(when (not (file-exists? USERS-LIST-FILE))
  (error 'users-rktd "expected to find users list in users.rktd (should probably \
just discover these from logs)."))

(define users
  (file->value (build-path here "users.rktd")))

;; this salt is used to map uids to hashed strings, just to allow me to
;; look at the data without making names obvious
(define uid-salt (file->bytes "/tmp/salt.txt"))

;; remove '=\r\n' from base64-encoding
(define (remove-last3 bytes)
  (match bytes
    [(regexp #px#"=\r\n$")
     (subbytes bytes 0 (- (bytes-length bytes) 3))]
    [(regexp #px#"\r\n$")
     (subbytes bytes 0 (- (bytes-length bytes) 2))]
    [other (raise-argument-error 'remove-last3
                                 "byte string ending with =\\r\\n or \\r\\n"
                                 0 bytes)]))

;; a map from users to anonymized ids
(define user-hash
  (for/hash ([u (in-list users)])
    (values u
            (~> u
                string->bytes/utf-8
                (bytes-append uid-salt #"a")
                sha224
                (subbytes 0 3)
                base64-encode
                remove-last3
                bytes->string/utf-8))))

(unless (= (length (hash-keys user-hash))
           (length (remove-duplicates (hash-values user-hash))))
  (error 'user-hash "hash collisions! two users have the same hash.\n"))

(define raw-lines
  (file->lines "/tmp/log10"))

;; map a line to either a session-start or a session-data (or false
;; if it's bogus)
(define (parse-line l)
  (match (regexp-match #px"^@([0-9a-f]{24}) (.*)" l)
    [#f (error 'parse-line "missing tai64 tag on line: ~v\n" l)]
    [(list _1 tai64n  content)
     (define readed (call-with-input-string content read))
     (match readed
       [(list 'session-start (? string? userid) (? string? session-key)
              (? string? training-str))
        (cond [(or (member userid bogus-users)
                   (member session-key bogus-session-keys))
               (printf "ignoring bogus session start with key ~v from user ~v\n"
                       session-key userid)]
              [else
               (session-start (~> tai64n
                                  parse-tai64n
                                  time-second)
                              (hash-ref user-hash userid)
                              session-key training-str)])]
       [(list 'session-data (? string? session-key) (? number? timestamp)
              (? number? box-num) (? string? box-content))
        (cond [(member session-key bogus-session-keys)
               (printf "ignoring bogus session data with key ~v"
                       session-key)]
              [else
               (session-data (time-second
                              (parse-tai64n
                               tai64n))
                             session-key timestamp box-num box-content)])]
       ;; old style, ignore:
       [(list 'session-start (or "guest" "clements" "guest2") (? string? sessionkey))
        #f]
       [(list 'session-data (or "guest" "clements")
              (? string? session-key) (? number? timestamp)
              (? number? box-num) (? string? box-content))
        #f]
       [other
        (printf "ignoring bogus line: ~v\n" l)
        #f])]))

;; parse all the lines, throw out the falses:
(define log-lines (filter (λ (x) x) (map parse-line raw-lines)))


(define session-starts
  (filter session-start? log-lines))

#;(for/list ([s (in-list session-starts)])
  (apply query-exec conn "INSERT INTO sessions VALUES (?,?,?,?)"
         (struct->list s)))

(call-with-output-file "/tmp/sessions.tsv"
  (lambda (port)
    (for ([s (in-list session-starts)])
      (display (apply format "~a\t~a\t~a\t~a\n" (struct->list s))
               port)))
  #:exists 'truncate)


(define session-datas
  (filter (λ (sd)
            (and
             (session-data? sd)
             (not (member (session-data-content sd)
                          bogus-boxentries))))
          log-lines))

(define session-datas-cleaned
  (sort
(apply
 append
 (for/list ([datas (group-by session-data-key
                            session-datas)])
  (define sorted
    (sort datas < #:key session-data-timestamp))
  (let loop ([datas datas] [prev #f])
    (cond [(empty? datas) empty]
          [else
           (define fst (first datas))
           (cond [(and prev
                       (equal? (session-data-boxnum fst)
                               (session-data-boxnum prev))
                       (equal? (session-data-content fst)
                               (session-data-content prev)))
                  (loop (rest datas) prev)]
                 [else
                  (cons fst (loop (rest datas) fst))])]))))
<
#:key session-data-server-timestamp))

(length session-datas)
(length session-datas-cleaned)

(call-with-output-file "/tmp/keystrokes.tsv"
  (lambda (port)
    (for ([s (in-list session-datas)])
      (display (apply format "~a\t~a\t~a\t~a\t~a\n"
                      (struct->list s))
               port)))
  #:exists 'truncate)

(call-with-output-file "/tmp/keystrokes-cleaned.tsv"
  (lambda (port)
    (for ([s (in-list session-datas-cleaned)])
      (display (apply format "~a\t~a\t~a\t~a\t~a\n"
                      (struct->list s))
               port)))
  #:exists 'truncate)

#;(disconnect conn)