#lang racket

(struct ttt (board moves))
(struct action (player position))

(define (generate-ttt-tree player1 player2)
  (define (generate-tree board player opponent)
    (ttt board (generate-moves board player opponent)))
  (define (generate-moves board0 player opponent)
    (define free-fields (board-find-free-fields board0))
    (for/list ((f free-fields))
      (define actnow (action player f))
      (define board1 (board-take-field board0 player f))
      (list actnow (generate-tree board1 opponent player ))))
  (generate-tree the-empty-board player1 player2))

