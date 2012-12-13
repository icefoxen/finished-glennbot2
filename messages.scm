; Okay, we're going to write functions to turn the IRC library messages into
; something less HIDEOUSLY STUPID.
;
; I'm thinking a list of (msgtype who-to-reply-to body), to start with.
; msgtype is one of: message, action, join, part/quit, nick...
(define foo 10)
