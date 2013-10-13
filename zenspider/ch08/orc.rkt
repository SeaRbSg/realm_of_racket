#lang racket

;; (require 2htdp/universe 2htdp/image)
;; (require data/queue)
;; (require racket/runtime-path)

(struct orc-world (player lom attack#))

(struct player (health agility strength))

(define MAX-HEALTH   35)
(define MAX-AGILITY  35)
(define MAX-STRENGTH 35)

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 8)

(struct monster (health) #:transparent)
(struct orc     monster (club)      #:transparent)
(struct hydra   monster ()          #:transparent)
(struct slime   monster (sliminess) #:transparent)
(struct brigand monster ()          #:transparent)

(orc-world (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH)
           (list (orc MONSTER-HEALTH0 (add1 (random CLUB-STRENGTH))))
           0)
