#lang racket

(require 2htdp/universe 2htdp/image)
;; (require data/queue)
;; (require racket/runtime-path)

(define MAX-HEALTH   35)
(define MAX-AGILITY  35)
(define MAX-STRENGTH 35)

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH   8)

(struct orc-world (player lom attack#))

(struct player (health agility strength) #:transparent #:mutable)

(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define player-health+
  (player-update! set-player-health!   player-health   MAX-HEALTH))
(define player-agility+
  (player-update! set-player-agility!  player-agility  MAX-AGILITY))
(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

(struct monster ([health #:mutable]) #:transparent)
(struct orc     monster (club)       #:transparent)
(struct hydra   monster ()           #:transparent)
(struct slime   monster (sliminess)  #:transparent)
(struct brigand monster ()           #:transparent)

;;; Misc Helpers

(define (interval- n m (max-value 100))
  (min (max 0 (- n m)) max-value))

(define (interval+ n m (max-value 100))
  (interval- n (- m) max-value))

(orc-world (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH)
           (list (orc MONSTER-HEALTH0 (add1 (random CLUB-STRENGTH))))
           0)
