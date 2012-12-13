(use irc)
(use posix)

(declare (unit util))

;;;;;;;;;;;;;;;;;;;;;;;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; So, irc:wait times out after a minute, probably because read-line does.
; I don't see how to change this time-out,
; And you can't select() off of Scheme streams.
; So, we do polling with a slight delay built into it so it
; doesn't devour the processor.
; However, posix sleep() only does integer values of seconds.
; And the Chicken posix library doesn't have usleep.
; So, we use select() to do the delay.
; As they say in the mit-scheme source code... HACK HACK HACK
; Hey, at least I'm not doing (system "sleep 0.1"), which is what
; I thought of first.
; And, sadly, I am told that this is actually The Right Way to do
; sub-second delays.  Oof.
(define (sleep-hack time)
  (file-select #f #f time))

(define (ignore2 x y) #f)

(define (is-channel? str) (char=? #\# (string-ref str 0))) 

(define (message-dest msg) (car (irc:message-parameters msg)))

(define (find f lst)
  (cond
    ((null? lst) #f)
    ((f (car lst)) (car lst))
    (else (find f (cdr lst)))))

(define (fold-left-y f x tsil) 
  (cond 
    ((null? tsil) x) 
    (else 
      (define (g x-) 
        (fold-left-y f x- (cdr tsil))) 
      (f x (car tsil) g))))

; *data-dir* is just configuration.
(define *data-dir* "data")

(define (data-file-name server datafile)
  (make-pathname (list *data-dir* datafile)))


; Handy for debugging.
(define (dump-message msg)
  (let ((time (seconds->string (current-seconds)))
        (sender (irc:message-sender msg))
        (receiver (irc:message-receiver msg))
        (prefix (irc:message-prefix msg))
        (command (irc:message-command msg))
        (timestamp (irc:message-timestamp msg))
        (code 'bop) ; (irc:message-code msg))
        (body (irc:message-body msg))
        (param (irc:message-parameters msg))
        (index (irc:message-index msg)))
    (format #t "Message recieved.
    Time: ~S, timestamp ~S
    Sender: ~S
    Receiver: ~S
    Prefix: ~S
    Command: ~S
    Code: ~S
    Index: ~S
    Param: ~S
    Body: ~S~%"
    time timestamp sender receiver prefix command code index param body)))



