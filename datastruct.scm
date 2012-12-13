; We have a ring buffer for the message input queue, then we have a list and a
; map for other stuffs
(declare (unit datastruct)
         (uses util))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  RING BUFFER  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; An array-based ring-buffer
(define-record buffer
	       buff
	       ptr)

(define-record-printer (buffer b out)
                       (format out "B(~S, ~S)"
                                (buffer-ptr b) (buffer-buff b)))
; Initialize new buffer.
(define (buffer-new size)
  (make-buffer (make-vector size "") 0))

(define (buffer-size b)
  (vector-length (buffer-buff b)))

; Add an item to the buffer, overwriting if necessary.
(define (buffer-add! b obj)
  (buffer-ptr-set! b (remainder (+ 1 (buffer-ptr b)) (buffer-size b)))
  (vector-set! (buffer-buff b)
	       (buffer-ptr b)
	       obj))

; Returns 1st item
(define (buffer-get b)
  (vector-ref (buffer-buff b) (buffer-ptr b)))

; Returns nth item, or an error if n > buffer length
(define (buffer-get-nth b n)
  (let ((blen (buffer-size b))
        (idx (- (buffer-ptr b) n)))
    (if (< blen n)
      (if (>= idx 0)
        (vector-ref (buffer-buff b) idx)
        (vector-ref (buffer-buff b) (+ blen idx)))
      (error (format #f "buffer-get-nth: n = ~S, but blen is only ~S!"
                     n blen)))))


; Returns last n items
(define (buffer-getn b n)
  (define (loop i accm)
    (if (< i 0)
      accm
      (loop (- i 1) (cons (buffer-get-nth b i) accm))))
  (if (< n (buffer-size b))
    (loop n '())
    (error (format #f "buffer-getn: n = ~S, but blen is only ~S!"
                   n blen))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LIST  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Okay, this is just a persistant unordered list of unique items.
; Sweeeeet.
; It works a lot nicer when you do it right.

(define (with-list lstname func)
  (list-dump (func (list-load lstname) lstname)))

(define (list-add lst)
  (with-list lst
             (lambda (l) 
               (if (find (lambda (x) (equal? x itm)) l)
                 l
                 (cons itm l)))))

(define (list-len lst)
  (length (list-load lst)))

(define (list-rem lst)
  (with-list lst
             (lambda (l)
               (remove (lambda (x) (equal? x itm)) l))))

(define (list-nth lst n)
  (XXX (list-load lst)))

(define (list-random lst)
  '())


(define (list-dump lst filename)
  (call-with-output-file
    filename
    (lambda (out)
      (define (loop lst)
        (cond
          ((null? lst) #t)
          (else
            (format out "~A~%" (car lst))
            (loop (cdr lst)))))
      (loop lst))))

(define (list-load filename)
  (call-with-input-file
    filename read-lines))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TABLE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A mapping of key-value pairs.
