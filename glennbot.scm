(use irc)
(use posix)
(use tcp)

(declare (uses channel)
         (uses util))



;;;;;;;;;;;;;;;;;;;;;;;;; Handler functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This function takes the irc library msg type and turns it into something
; more reasonable.
; Fields: type, from, who-to-message-back-at, body
; Possible types: message, action, join, leave, nickchange, ping, modechange?
(define-record irc-message 
               type from replyto body)

(define-record-printer (irc-message msg out)
   (format out "Msg type '~S' from '~A', reply to '~A': '~A'"
           (irc-message-type msg) (irc-message-from msg)
           (irc-message-replyto msg) (irc-message-body msg)))

(define (string-channel? str)
  (char=? #\# (string-ref str 0)))

; This function is used to find the appropriate value for irc-message-replyto
; Ugh. Basically, if the message is to a channel, we reply to a channel, but 
; if the message is to me, then we reply to the person who sent it.
(define (wrangle-reply msg)
  (let ((from (irc:message-sender msg))
        (to (irc:message-receiver msg)))
    (if (string-channel? to)
      to 
      from)))


; Sigh.  I don't seem to be gaining much by using this IRC library over 
; writing my own.
;
; Okay...  PRIVMSG.
; command is the command.  That's simple.
; sender is who sent it.  Also simple.
; Then param.  (car param) is either the channel or the bot name.  (cadr
; param) is either a string or an extended data object.
; Gods this library is terrible.
; The protocol ain't too hot either.
(define (parse-message msg)
  (let ((command (irc:message-command msg))
        (from (irc:message-sender msg))
        (to (irc:message-receiver msg))
        (param (irc:message-parameters msg))
        (replyto (wrangle-reply msg)))
    (cond
      ((string=? command "PRIVMSG")
       (if (irc:extended-data? (cadr param))
         (make-irc-message 'action from replyto
                           (irc:extended-data-content (cadr param)))
         (make-irc-message 'message from replyto (cadr param))))

      ; Sender = person, receiver = channel 
      ((string=? command "JOIN")
       (make-irc-message 'join from (irc:message-receiver msg) #f))

      ; Sender = person, receiver = channel, param = (channel pithy-message)
      ((string=? command "PART") 
       (make-irc-message 'part from to (cadr param)))

      ; Sender = nick from, receiver = nick to 
      ((string=? command "NICK") 
       (make-irc-message 'nick from #f to))

      ; Param = (channel flag person)
      ; EXCEPT when the mode gets initially set by the channel!
      ; Yippie!
      ;((string=? command "MODE") 
      ; (make-irc-message 'mode from (caddr param) (cadr param)))

      ; Receiver = source (that totally makes sense)
      ((string=? command "PING") 
       (make-irc-message 'ping to #f #f)))))


; Processes a single message: Parses messages, responds to pings, puts 
; appropriate things in appropriate channel queues and invokes handlers, and 
; so on.
(define (process-message con message channels)
  (if message (format #t "~S~%" message))
  (let ((channel (assoc (irc-message-replyto message) channels)))
    ; Ugh.  This, in principle, should 
    (if channel
      (begin (channel-input channel message) channels)
      (let ((c (channel-new con (irc-message-replyto message))))



  



; Polling loop.  Reads all the messages it can until there are none left, 
; then waits a little before trying again. 
; It works quite nicely.
(define (listenloop con channels)
  (let ((message (irc:listen con)))
    (if message
      (begin (process-message con (parse-message message) channels)
             ;(format #t "Processing~%")
             ;(flush-output (irc:connection-out con))
             (flush-output (current-output-port)))
      (sleep-hack 0.1))
    (listenloop con channels)))

(define (handle-args)
  (if (= 0 (length (argv)))
    (begin 
      (format #t "Usage: glennbot nick server channels...~%")
      (exit 1))
    (cdr (argv))))


(define (main)
  ;(tcp-buffer-size 0)
  (let* ((options (handle-args))
         (con (irc:connection nick: (car options) server: (cadr options))))


    (format #t "Made connection.~%")
    (irc:connect con)
    (format #t "Connected.~%")
    (define channels (map (lambda (x) (channel-new con x)) (cddr options)))
    (format #t "Channels defined.~%")

    ;(add-handlers con)
    (format #t "Handlers added~%")

    (listenloop con channels)))


(main)
