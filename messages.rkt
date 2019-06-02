#lang racket

(require "rooms.rkt")

(define rooms-to-messages (make-hash))

(define max-messages 10)

(define (add-message messages new-message)
  (cond [ (>= (length messages) max-messages)
          (append (take messages (- max-messages 1)) (list new-message))]
        [ else (append messages (list new-message))]))

(define (update-messages room-name message)
  (define selected-room-id (get-room-id-by-name room-name (get-rooms)))
  (hash-set! rooms-to-messages
             selected-room-id
             (add-message (hash-ref rooms-to-messages selected-room-id '()) message)))

(define (get-messages room-name)
  (hash-ref rooms-to-messages (get-room-id-by-name room-name (get-rooms))))

(define (display-messages room-name)
  (for-each
   (lambda (message idx) (writeln (string-append "message " (number->string idx) ": " message)))
   (get-messages room-name)
   (reverse (count-down (length (get-messages room-name))))))

(define (count-down n)
  (cond [(equal? 0 n) '()]
        [else (cons n (count-down (- n 1)))]))


