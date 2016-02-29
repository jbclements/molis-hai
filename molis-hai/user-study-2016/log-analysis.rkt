#lang racket

(require racket/runtime-path
         sha
         net/base64
         threading
         math/statistics
         levenshtein
         "string-editing.rkt"
         "table.rkt"
         plot
         rackbot/tai64n
         srfi/19
         racket/block
         racket/struct)


;; these users are just a consequence of testing... ignore them.
(define bogus-users
  (list "consenter"
        "nonconsenter"))

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
  (error 'users-rktd "expected to find users list in users.rktd (should probably just discover these from logs)."))

(define users
  (file->value (build-path here "users.rktd")))

;; this salt is used to map uids to hashed strings, just to allow me to
;; look at the data without making names obvious
(define uid-salt #"BJ\313\374\342")

;; remove '=\r\n' from base64-encoding
(define (remove-last3 bytes)
  (match bytes
    [(regexp #px#"=\r\n$")
     (subbytes bytes 0 (- (bytes-length bytes) 3))]
    [other (error 'remove-last3
                  "unexpected last 3 chars")]))

;; a map from users to anonymized ids
(define user-hash
  (for/hash ([u (in-list users)])
    (values u
            (~> u
                string->bytes/utf-8
                (bytes-append uid-salt #"a")
                sha224
                (subbytes 0 2)
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
               (printf "ignoring bogus session with key ~v from user ~v\n"
                       session-key userid)]
              [else
               (session-start (~> tai64n
                                  parse-tai64n
                                  time-second)
                              (hash-ref user-hash userid)
                              session-key training-str)])]
       [(list 'session-data (? string? session-key) (? number? timestamp)
              (? number? box-num) (? string? box-content))
        (session-data (time-second
                       (parse-tai64n
                        tai64n))
                      session-key timestamp box-num box-content)]
       ;; old style, ignore:
       [(list 'session-start (or "guest" "clements" "guest2") (? string? sessionkey))
        #f]
       [(list 'session-data (or "guest" "clements") (? string? session-key) (? number? timestamp)
              (? number? box-num) (? string? box-content))
        #f]
       [other
        (printf "ignoring bogus line: ~v\n" l)
        #f])]))

;; parse all the lines, throw out the falses:
(define log-lines (filter (λ (x) x) (map parse-line raw-lines)))

(define session-starts
  (make-table
   session-start-col-names
   (map struct->list (filter session-start? log-lines))))

(define session-keys
  (in-table-column session-starts 'key)
  #;(map session-start-key session-starts))

(define session-datas
  (make-table
   session-data-col-names
   (map
    struct->list
    (filter (λ (sd)
              (and
               (session-data? sd)
               (not (member (session-data-content sd)
                            bogus-boxentries))))
            log-lines))))

(printf "~v session starts\n" (table-row-count session-starts))

#;(plot
 (density
  (map string-length (hash-values uid->training-str-table))
  0.05))

;; looks like 13 is between the lengths of the two password lengths
(define PASSWORD-LENGTH-THRESHOLD 13)




;; SHOW THE DENSITY OF THE SESSION DATES
(define session-dates
  (in-table-column session-starts 'server-timestamp))
(printf "density plot of session start times:\n")
(plot (density session-dates 0.1)
      #:width 1000)

(plot-file (density session-dates 0.01)
           "/tmp/log-density.pdf"
      #:width 1000)
(plot (density session-dates 0.01)
      #:width 1000)


(printf "total lines of session data: ~v\n" (table-row-count session-datas))

(define timestamp-lists
  (sort
   (for/list ([uid (in-table-column session-starts 'userid)])
     (sort (table-ref session-starts 'userid 'server-timestamp uid) <))
   <
   #:key last))

(plot
 (apply
  append
  (for/list ([t-list (in-list timestamp-lists)]
             [i (in-naturals)])
    (define p
      (for/list ([t (in-list t-list)])
        (vector t i)))
    (list (points p)
          (lines p))))
 #:width 1000)



;; map from session key to user:
#;(define key->user-map
  (for/hash ([s (in-list session-starts)])
    (values (session-start-key s) (session-start-userid s))))

;; session keys with no data:
(define no-session
  (remove* (in-table-column session-datas 'key) session-keys)
  #;(filter (λ (k) (not (hash-has-key? data k))) session-keys))

(printf "~v session keys have no data\n" (length no-session))

(define good-session-keys
  (in-table-column session-datas 'key))

#;(define good-session-starts
  ()
  (filter (λ (start) (member (session-start-key start) good-session-keys))
          session-starts))

;; map from uid to training-str
#;(define uid->training-str-table
  (for/hash ([g (group-by session-start-userid session-starts)])
    (define strs (map session-start-training-str g))
    (define userid (session-start-userid (first g)))
    (unless (equal? (length (remove-duplicates strs)) 1)
      (error 'training-str-table "more than one training string for user: ~v\n"
             userid))
    (values userid (first strs))))

;; group data by session key
#;(define data
  (for/fold ([ht (hash)])
            ([d (in-list session-datas)])
    (cond
      [(member (session-data-key d) session-keys)
       (hash-set ht (session-data-key d)
                 (cons d (hash-ref ht (session-data-key d) empty)))]
      [else ht])))

;; ensure all 5 boxes filled out, no paste operations.
;; produces a list of lists of datas. All are in chonological
;; order
(define (clean-session lines)
  ;; these will be reversed as they go into the hash bins:
  (define by-time (sort lines > #:key session-data-timestamp))
  (define by-nth
    (for/fold ([ht (hash)])
              ([l (in-list by-time)])
      (hash-set ht (session-data-boxnum l)
                (cons l (hash-ref ht (session-data-boxnum l)
                                  '())))))
  (define boxes-with-data (sort (hash-keys by-nth) <))
  ;; check all boxes were completed:
  #;(unless (equal? boxes-with-data
                  '(1 2 3 4 5))
    (error 'clean-session
           "session ~v has incomplete set of box-nums: ~v"
           (session-data-key (first lines))
           (hash-keys by-nth)))
  ;; check box 1 is present
  (unless (= (first boxes-with-data) 1)
    (error 'clean-session
           "session ~v is missing box 1"
           (session-data-key (first lines))))
  ;; check the boxes occur in numeric order
  (for/list ([a (in-list boxes-with-data)]
             [b (in-list (rest boxes-with-data))])
    (unless (= (add1 a) b)
      (error 'clean-session
             "session ~v has incomplete box-nums: ~v"
             (session-data-key (first lines))
             boxes-with-data)))
  ;; warn if missing late boxes:
  (when (< (length boxes-with-data) 5)
    (printf "warning: session ~v has data for only ~v boxes\n"
            (session-data-key (first lines))
            (length boxes-with-data)))
  (define last-box-num (last boxes-with-data))
  (for/list ([i (in-list boxes-with-data)])
    ;; ensure chronological order is preserved:
    (when (< i last-box-num)
      (define earliest-of-next
        (session-data-timestamp (first (hash-ref by-nth (add1 i)))))
      (for ([l (in-list (hash-ref by-nth i))])
        (unless (< (session-data-timestamp l) earliest-of-next)
          (error 'out-of-order))))
    ;; eliminate lines whose strings are equal to the previous one:
    (define deduped
      (let loop ([prev #f] [remaining (hash-ref by-nth i)])
        (cond [(empty? remaining) '()]
              [else
               (define f (car remaining))
               (define fc (session-data-content f))
               (cond [(equal? prev fc)
                      (loop prev (rest remaining))]
                     [else
                      (when (string? prev)
                        (when (not (legal-change? prev fc))
                          (printf
                           "warning: illegal change (spellcheck?) from ~v to ~v in session: ~v\n"
                           prev fc
                           (session-data-key f))))
                      (cons f
                            (loop fc (rest remaining)))])])))
    (when (< 100 (length deduped))
      (error 'deduping "still too many entries in deduped list: ~v"
             deduped))
    deduped))

#;(
(define cleaned
  (for/list ([key (in-table-column session-datas 'key)])
    (vector key (clean-session (table-ref session-datas 'userid )))))
;; a map from session key to cleaned data
(define cleaned
  (for/hash ([(key session) (in-hash data)])
    (values key (clean-session session))))

;; a map from anonymized uids to session starts
(define uid-session-starts
  (for/fold ([ht (hash)])
            ([s (in-list session-starts)]
             #:when (hash-has-key? cleaned (session-start-key s)))
    (define fake-uid (session-start-userid s))
    (hash-set ht fake-uid
              (append (hash-ref ht fake-uid '())
                      (list (session-start-key s))))))

;; a map from anonymized uids to session datas
(define uid-sessions
  (for/hash ([(k v) (in-hash uid-session-starts)])
    (values k (map (λ (key) (hash-ref cleaned key)) v))))

;; a list of how many sessions each user has
(define session-nums (map length (hash-values uid-sessions)))
(define max-num-sessions (apply max session-nums))
(for ([i (in-range (add1 max-num-sessions))])
  (printf "users with ~v sessions: ~v\n"
          i
          (length (filter (λ (n) (= n i)) session-nums))))

(printf "mean sessions per uid: ~v"
        (mean (map length (hash-values uid-sessions))))

;; plot the times of participation of each user
(block
 (define start-sets (for/list ([(uid sessions) (in-hash uid-session-starts)])
                      (map session-start-server-timestamp sessions)))
 (sort start-sets < #:key last))

;; given a list of 
(define ((box-dist training-str) datas)
  (define key (session-data-key (last datas)))
  (define last-entry (session-data-content (last datas)))
  (list last-entry (string-levenshtein training-str last-entry)))


;; printout of first and finals
#;(for/list ([(uid sessions) (in-hash uid-sessions)])
  (define str (hash-ref uid->training-str-table uid))
  (list uid str (map (box-dist str) (map first sessions))))


#;(plot
 #:width 1300
 (apply
  append
 (for/list ([user-starts (group-by session-start-userid good-session-starts)]
            [idx (in-naturals)])
   (define training-str (hash-ref uid->training-str-table (session-start-userid
                                                             (first
                                                              user-starts))))
   (define data
     (for/list ([session (in-list
                        (sort user-starts < #:key session-start-server-timestamp))])
     (define last-entry
       (last (first (hash-ref cleaned (session-start-key session)))))
     
     (vector (+ (session-start-server-timestamp session) (random 300))
             (+ (/ (- (random 20) 10) 20)
                (string-levenshtein (session-data-content last-entry)
                                    training-str)))))
   (list
    (lines data
           #:color (cond [(< (string-length training-str) PASSWORD-LENGTH-THRESHOLD) 0]
                         [else 1]))
    (points data)))))


#;(
(for/list ([key (in-list session-keys)]
           
           #:when (hash-ref data key #f))
  (define s (hash-ref data key))
  (map length (sort (group-by fifth s) < #:key (λ (x) (fifth (first x)))))
  #;(first (sort s > #:key session-data-timestamp))
  )

(define zzpass (findf (λ (x) (equal? (fourth x) "0vC8R3lvzro")) session-starts))
(define zz (sort (hash-ref data "0vC8R3lvzro") < #:key fourth))
(call-with-output-file "/tmp/attempts.txt"
  #:exists 'truncate
  (λ (port)
    (for ([l (in-list (map sixth zz))])
      (fprintf port "~a\n" l))))))