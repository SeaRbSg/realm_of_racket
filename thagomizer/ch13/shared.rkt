#lang racket
(provide UPPER LOWER)

#|
In the Distributed Guess My Number game, a player uses a client
to connect to the server. The server attempts to guess what
number the player is thinking of. Each time the server guesses,
the player must use the arrow keys to tell the server if it is
too small or too large.

A StoCMessage, a server client message, is any natural number
between LOWER and UPPER (inclusive). The numbers represent the
guess.

A CtoSMessage, a client to server message, is one of the
following two strings:
-- "up"
-- "down"
with the obvious meaning.
|#

(define UPPER 100)
(define LOWER 0)
