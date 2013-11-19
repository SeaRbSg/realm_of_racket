#lang racket
(provide
 id?
 id=?
 (struct-out player)
 (struct-out body)
 get-score
 PLAYER-FATTEN-DELTA
 WIDTH
 HEIGHT
 CUPCAKE
 PLAYER-SIZE
 SCORE
 GOTO
 SERIALIZE
 GOTO-LENGTH)

;; player
; id is player id assigned by the server
; body is the location and size of the player
; waypoints is an lifo list of waypoints
(struct player (id body waypoints) #:prefab)

;; body
; size is a positive integer
; loc is a complex number that represents location
(struct body (size loc) #:prefab #:mutable)

(define id? string?)
(define id=? string=?)

(define SCORE 'score)
(define SERIALIZE 'state)
(define GOTO 'goto)
(define GOTO-LENGTH 3)

(define WIDTH 1000)
(define HEIGHT 700)
(define CUPCAKE 15)
(define PLAYER-SIZE (* 3 CUPCAKE))
(define PLAYER-FATTEN-DELTA 5)

(define (get-score f)
  (/ (- f PLAYER-SIZE) PLAYER-FATTEN-DELTA))
