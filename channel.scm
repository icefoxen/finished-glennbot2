; A 'channel' structure contains the channel buffer, has functions to get
; and set properties of the channel (op, users, name, title, etc)
; and has whatever other info on the channel that we need.
(use irc)
(require-extension srfi-13)
(declare (unit channel)
	 (uses datastruct)
         (uses util))

(define-record channel
	       name
	       buffer
	       conn
               commands)

(define-record-printer (channel chan out)
   (format out "Channel<#~A, ~S>" (channel-name chan) (channel-buffer chan)))

(define (channel-conversation-new conn channelname)
  (let ((ch (make-channel channelname (buffer-new 16) conn '())))
    (channel-init-commands ch)
    (format #t "Starting conversation with: ~S~%" channelname)
    ch))

(define (channel-new conn channelname)
  (let ((ch (make-channel channelname (buffer-new 16) conn '())))
    (channel-init-commands ch)
    (format #t "Joining: ~S~%" channelname)
    (irc:join conn channelname)
    ;(irc:add-message-handler! 
    ;  conn (lambda (msg) (channel-input ch msg) #f) tag: channelname
    ;  receiver: channelname)

    ch))


; Make this toggle a flag on the channel object, and the mainloop go through
; to check for it and remove or reconnect dc'd channels.
(define (channel-quit ch message)
  (irc:remove-message-handler! (channel-conn ch) (channel-name ch))
  (irc:part (channel-conn ch) (channel-name ch))) ; message))


; Channel info and settings
; ...might need to do relatively low-level protocol hacking for this...
; MODE flags for op and voice, TOPIC to do topic, USERS to find users...
(define (channel-op ch usr)
  (irc:command (string-append "MODE +o :" usr)))

(define (channel-deop ch usr)
  (irc:command (string-append "MODE -o :" usr)))

(define (channel-voice ch usr)
  (irc:command (string-append "MODE +v :" usr)))

(define (channel-devoice ch usr)
  (irc:command (string-append "MODE -v :" usr)))

(define (channel-set-topic ch topic)
  (irc:command (string-append "TOPIC " (channel-name ch) " :" topic)))

(define (channel-topic ch)
  (irc:command (string-append "TOPIC " (channel-name ch) " :")))

; XXX: Banana hammock.  I hate the IRC protocol.
(define (channel-users ch) '())

(define (channel-nick ch)
  (irc:connection-nick (channel-conn ch)))

; Channel I/O
(define (channel-say ch str)
  (irc:say (channel-conn ch) str (channel-name ch)))

(define (channel-action ch str)
  (irc:action (channel-conn ch) str (channel-name ch)))

; This puts a message through the works: Log, queue, parse, respond.
(define (channel-input ch msg)
  (channel-log ch msg)
  (channel-buffer-add ch msg)
  (channel-do-commands ch)) 

; This sticks a message in the ring buffer.
(define (channel-buffer-add ch msg)
  (buffer-add! (channel-buffer ch) msg))

; Every time we recieve a new line, we go through and parse the whole command
; backlog.  That way we don't have to keep a state machine around.
(define (channel-do-commands ch) 
  (format #t "Doing commands...~%")
  (define (loop rest)
    (if (not (null? rest))
      (begin
        (if ((caar rest) ch)
          ((cdar rest) ch))
        (loop (cdr rest)))))
  (loop (channel-commands ch)))

; Log stuff to file.
(define (channel-log ch msg) 
  (do-log (channel-conn ch) msg))



;;;;;;;;;;;;;;;;;;; Command module support funcs ;;;;;;;;;;;;;;;;;;;;;;;
; Hokay...
; Each channel can have a number of commands.
; A "command" is just a pair of (parse-func . process-func).
; Parse-func is fed the message buffer, and does arbitrary stuff.
; If it returns true, then process-func is called on the channel object.
; So all the pattern-recognition complexity gets kept in parse-func,
; at least, and we can write handy routines to parse in a consistant manner
; or handle whatever special cases we need to.

(define (channel-add-command ch parse-func process-func)
  (let ((commands (cons (cons parse-func process-func)
                        (channel-commands ch))))
    (channel-commands-set! ch commands))
  (format #t "Command added: ~S~%" (channel-commands ch)))

(define last-message
  (compose buffer-get channel-buffer))
;  (buffer-get (channel-buffer c)))


; This function takes the irc library msg type and turns it into something
; more reasonable.
; Fields: type, from, who-to-message-back-at, body
; Possible types: message, action, join, leave, nickchange
(define-record irc-message
               type
               from
               returnto
               body)

(define (string-channel? str)
  (char=? #\# (string-ref str 0)))

; Ugh.  Basically, if the message is to a channel, we reply to a channel, but if
; the message is to me, then we reply to the person who sent it.
(define (wrangle-reply msg)
  (let ((from (irc:message-sender msg))
        (to (irc:message-receiver msg)))
    (if (string-channel? to)
      to
      from)))

(define (parse-irc-message msg)
  (let ((command (irc:message-command msg))
        (from (irc:message-sender msg))
        (to (irc:message-receiver msg))
        (extended (irc:extended-data? msg)))
    (cond
      ((string=? command "JOIN")
       (make-irc-message 'join from (wrangle-reply msg) #f))
      ((string=? command "PART") '())
      ((string=? command "NICK") '())
      ((string=? command "PRIVMSG") '()))))

; Type == "PRIVMSG" or something
(define (msg-is msg type)
  (equal? (irc:message-command msg) type))

; XXX: Make this better
(define (msg-string msg)
  ;(format #t "MS parameters: ~S~%" (irc:message-parameters msg))
  (cadr (irc:message-parameters msg)))

; XXX:
; This doesn't quite work 'cause it can't create and register a new channel
; object.
; ...dur.  We just need a function in glennbot.scm to REGISTER it!
; But, do we care?
;(define (command-join c)
;  (let ((parse-func
;          (lambda (c)
;            (and (string-contains-ci (last-message c) (channel-nick c))
;                 (string-contains-ci (last-message c) "join"))))
;        (process-func
;          (lambda (c)
;            (irc:join (channel-conn c) 
;
;    (channel-add-command c parse-func process-func)))

; XXX: Okay, this EXPLODES spectacularly on actions.
(define (privmsg-with-strings? msg . strs)
  (if (msg-is msg "PRIVMSG")
    (letrec ((loop 
            (lambda (rest)
              (cond
                ((null? rest) #t)
                ((string-contains-ci (msg-string msg) (car rest))
                 (loop (cdr rest)))
                (else #f)))))
      (loop strs))
    #f))



;;;;;;;;;;;;;;;;;;;;;;;;;; Command modules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (channel-init-commands c)
;  (command-quit c)
  (command-cookie c)
  (command-hug c)
  (command-die c))

;(define (command-quit c)
;  (let ((parse-func
;          (lambda (c)
;            (let ((msg (last-message c)))
;              (privmsg-with-strings? msg (channel-nick c) "flop"))))
;
;        (process-func
;          (lambda (c)
;            (format #t "Leaving ~S~%" (channel-name c))
;            (channel-quit c "Arr, mateys"))))
;
;    (channel-add-command c parse-func process-func)))

(define (command-die c)
  (let ((parse-func
          (lambda (c)
            (let ((msg (last-message c)))
              (and (equal? (car (irc:message-prefix msg)) "Icefox")
                   (privmsg-with-strings? msg (channel-nick c) "die")))))

        (process-func
          (lambda (c)
            (irc:quit (channel-conn c) "Aieee!")
            (format #t "Quitting...~%")
            (exit 0))))

    (channel-add-command c parse-func process-func)))

(define (command-hug c)
  (let ((parse-func
          (lambda (c)
            (let ((msg (last-message c)))
              (privmsg-with-strings? msg (channel-nick c) "hug"))))

        (process-func
          (lambda (c)
            (let ((from (car (irc:message-prefix (last-message c)))))
              (channel-say c "Woohoo!")
              (channel-action c (string-append "hugs " from " back!"))))))

    (channel-add-command c parse-func process-func)))

(define (command-cookie c)
  (let ((parse-func
          (lambda (c)
            (let ((msg (last-message c)))
              (privmsg-with-strings? msg (channel-nick c) "cookie"))))

        (process-func
          (lambda (c)
            (let ((from (car (irc:message-prefix (last-message c)))))
              (channel-say c "Anna, cookie.")
              (channel-say c "Yay, cookies!")
              (channel-action c "noms.")))))

    (channel-add-command c parse-func process-func)))

(define (command-poke c)
  0)

; Word of the day.
(define (command-wotd c)
  0)

(define (command-name c)
  0)

; New, edit, print, add, delete
(define (command-list c)
  0)



;;;;;;;;;;;;;;;;;;;;;;;;; Logging functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; XXX: Someday we might want to rip this all apart and replace it with
; something less annoying.  Starting off with a third-order function might
; be a bad idea.

; This creates a function that takes a logger function and a message.
; The function does a (format) with the given string, with the arguments being
; the time, the message sender, and the channel name.
; Hence tsc.  Time, sender, channel.
; ...yeah.  You'll see.  
; (string -> ((string -> string -> unit) -> message -> unit)).  Whee!
; (car param) gets the channel name out of the parameter list, by the way.
(define (make-tsc-logger formatstring)
  (lambda (logger msg)
    (let ((time (seconds->string (current-seconds)))
          (sender (irc:message-sender msg))
          (dest (message-dest msg)))
      (logger dest
               (format #f formatstring time sender dest)))))

(define log-join (make-tsc-logger "~A: ~A has joined ~A~%"))
(define log-part (make-tsc-logger "~A: ~A has left ~A~%"))
(define log-quit (make-tsc-logger "~A: ~A has quit:~A~%"))

; We don't log private messages, but do log messages and actions.
; While there are technically +!& channels, we don't worry about 'em.
(define (log-privmsg logger msg)
  (if (is-channel? (message-dest msg)) 
    (let ((time (seconds->string (current-seconds)))
	  (sender (irc:message-sender msg))
	  (dest (message-dest msg))
	  (message (cadr (irc:message-parameters msg))))
      (if (irc:extended-data? message)
	(if (equal? 'ACTION (irc:extended-data-tag message))
	  (logger dest 
		   (format #f "~A: ~A ~A~%" 
			   time sender (irc:extended-data-content message)))
	  (logger dest 
		   (format #f "~A: Unknown extended data from ~S: ~A ~A~%" 
			   time sender
			   (irc:extended-data-tag message) 
			   (irc:extended-data-content message))))
	(begin 
	  (logger dest (format #f "~A: <~A> ~A~%" time sender message)))))))


(define (date-string)
  (let ((localtime (seconds->local-time (current-seconds))))
    (format #f "~A-~A-~A" 
            (+ 1900 (vector-ref localtime 5)) (vector-ref localtime 4)
            (vector-ref localtime 3))))

(define (output-file-name server channelname) 
  (make-pathname 
    (list *data-dir* server) 
    (string-append channelname "-" (date-string)) "log"))

; This creates a logger, which is a function that takes a channel name
; and log text, and writes it out to the correct file.
; See, we open and close the file with each write, so things stay flushed.
; And we ALSO regenerate the output-file-name from scratch each time, so
; it makes dates and files as necessary with each new message, without 
; having to check if it's changed to a new day or such.
; And, Chicken >> R5RS
(define (make-logger con)
  ; XXX
  ; Check for existance of directories, and creat them if necessary here.
  (lambda (channel logtext)
    (call-with-output-file 
      (output-file-name (irc:connection-server con) channel)
      ;(lambda (logport) (display logtext logport))
      (lambda (logport) 
	;(display "BAH\n")
	;(format #t "TWO: ~S ~S ~S~%" format logport logtext)
	;(format logport logtext))
	;(format #t "ONE: ~S~%~S~%~S~%~S~%" channel format logtext logport)
	(format logport logtext))
      append:)))


; FUNCTION DISPATCH IN A COND
; BECAUSE I CAN
; XXX: Seperate logfiles on command for things like RP's?
; Maybe something along the lines of...
; (define logger-alist `(("JOIN" ,log-join) 
;    ("PART" ,log-part) ...)) 
; (define this-logger (cond ((assoc command logger-alist) => cadr) 
;    (else (lambda ...)))
(define (do-log con msg)
  (let ((command (irc:message-command msg))
        (logger (make-logger con)))
    ((cond
      ; Notices?  Modes?  We can ignore pings, at least...
      ((equal? command "JOIN") log-join)
      ((equal? command "PART") log-part)
      ((equal? command "QUIT") log-quit)
      ((equal? command "PRIVMSG") log-privmsg)
      ((equal? command "PING") ignore2)
      (#t (lambda (lf msg) format #t "Some message: ~S~%" command)))
     logger msg)))
