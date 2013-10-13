#lang racket
(require 2htdp/image 2htdp/universe)
(require rackunit rackunit/text-ui)

;;
;; -- STRUCTS --------------------------------------
;;
(struct orc-world (play lom attack#) #:transparent)
(struct player (health agility strength) #:transparent)
(struct monster ([health #:mutable]) #:transparent)
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)

;;
;; -- CONSTANTS ------------------------------------
;;

;; Player
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

;; Monster
(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 8)
