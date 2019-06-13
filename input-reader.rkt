#lang racket

(require "rooms.rkt")
(require "messages.rkt")

(provide read-syntax)

(define (read-syntax src in)
  (read-command in))

(define (read-command input)
  (select-command (string-split input)))

(define-syntax-rule (select-command command-args)
  (let ([command-name (first command-args)])
    (cond [(equal? command-name "help") (handle-help)]
          [(equal? command-name "list-rooms") (handle-list-rooms)]
          [(equal? command-name "display-room") (handle-display-room (second command-args))]
          [(equal? command-name "solve-room") (handle-solve-room
                                               (second command-args)
                                               (third command-args))]
          [(equal? command-name "check-messages") (handle-check-messages (second command-args))]
          [(equal? command-name "add-message") (handle-add-message
                                                (second command-args)
                                                (string-join (cdr (cdr command-args)) " "))]
          [else (handle-help)])))

(define (handle-help)
  (writeln "The allowed commands are:")
  (writeln "help")
  (writeln "list-rooms")
  (writeln "display-room example-room")
  (writeln "solve-room example-room example-answer")
  (writeln "check-messages example-room")
  (writeln "add-message example-room example-message"))

(define (handle-list-rooms)
  (list-rooms (get-rooms)))

(define (handle-display-room room)
  (display-selected-room room))

(define (handle-solve-room room answer)
  (solve-selected-room room answer))

(define (handle-check-messages room)
  (display-messages room))

(define (handle-add-message room message)
  (update-messages room message))


