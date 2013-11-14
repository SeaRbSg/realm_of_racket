#lang racket

(provide id? id=?
         (struct-out player)
         (struct-out body)
         get-score
         PLAYER-FATTEN-DELTA
         WIDTH HEIGHT CUPCAKE PLAYER-SIZE
         SCORE GOTO SERIALIZE
         GOTO-LENGTH)

(struct player (id body waypoints) #:prefab)
(struct body (size loc) #:prefab #:mutable)

;; junk missing from the book

(define id?  string?)
(define id=? string=?)

(define SCORE               'score)
(define SERIALIZE           'state)
(define GOTO                'goto)
(define GOTO-LENGTH         3)
(define WIDTH               1000)
(define HEIGHT              700)
(define CUPCAKE             15)
(define PLAYER-SIZE         (* 3 CUPCAKE))
(define PLAYER-FATTEN-DELTA 5)

(define (get-score f)
  (/ (- f PLAYER-SIZE) PLAYER-FATTEN-DELTA))
