#lang racket
;;Chapter 8 ORC Game

;;Constants
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

;;Init structs
(struct orc-world (player lom attack#))
(struct player (health agility strength) #:mutable)
(struct monster ([health #:mutable]))
(struct orc monster (club))
(struct hydra ())
(struct slime (sliminess))
(struct brigand ())

(define (stab-orc an-orc)
  (set-monster-health! an-orc (- (monster-health an-orc) DAMAGE)))

(define (player-health+ player delta)
  (define nih (interval+ (player-health player) delta HEALTH))
  (set-player-health! player nih))

(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))