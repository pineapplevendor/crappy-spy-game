#lang racket

(struct message (text) #:transparent)

(struct room (name clue answer messages) #:transparent)

(define (string->integers str)
  (map char->integer (string->list str)))

(define (integers->string ints)
  (list->string (map integer->char ints)))

(define (shift-string str shift)
  (integers->string (map (lambda (n)
                           (+ n shift))
                         (string->integers str))))

(define easy-1 (room
                "Did you forget?"
                (string-append "decode " (shift-string "password" 3))
                "password"
                '()))

(define easy-2 (room
                "Wrong way"
                (string-append "decode " (shift-string "reverse" -1))
                "reverse"
                '()))

(define easy-3 (room
                "Who is the spy?"
                "There are two engineers and one is a spy.
The spy always lies and the other does not. Engineer Roy says,
'We can both be trusted'
and engineer Bartholomew says,
'Engineer Roy is a spy.'
Are Roy, Bartholomew, or neither the spy?"
                "Roy"
                '()))

(define medium-1 (room
                "Alibi confusion"
                "The innocent will not lie or provide a false alibi.
Find the smallest possible group of liars who provide an alibi to each other for the murder.
Provide the killers' names, in alphabetic order, separated by spaces.
Catherine provides an alibi for Elizabeth.
Catherine provides an alibi for Alice.
Elizabeth provides an alibi for Florence.
Florence provides an alibi for Alice.
Elizabeth provides an alibi for Catherine.
Danielle provides an alibi for Alice.
Alice provides an alibi for Florence.
Barbara provides an alibi for Danielle.
Alice provides an alibi for Barbara."
                "Catherine Elizabeth"
                '()))

(define rooms (list easy-1 easy-2 easy-3 medium-1))

(define (list-rooms rooms)
  (map (lambda (room) (room-name room)) rooms))

(define (select-room selected-name rooms)
  (first (filter (lambda (room) (eq? (room-name room) selected-name)) rooms)))

(define (display-room room)
  (writeln (string-append "room: " (room-name room)))
  (writeln (string-append "clue: " (room-clue room))))
  
(define (solves-room? room answer)
  (eq? (room-answer room) answer))

(define (solve-room room answer)
  (writeln (cond
             [(solves-room? room answer) (string-append "You solved room: '" (room-name room) "'")]
             [else "Sorry, that's not the answer"])))


  
  





