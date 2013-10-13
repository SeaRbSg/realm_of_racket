#lang racket

(require 2htdp/universe 2htdp/image)

;;; Constants

(define-namespace-anchor namespace-anchor)
(define my-namespace (namespace-anchor->namespace namespace-anchor))

(define MAX-HEALTH   35)
(define MAX-AGILITY  35)
(define MAX-STRENGTH 35)

(define ATTACKS#     4)
(define STAB-DAMAGE  2)
(define FLAIL-DAMAGE 3)
(define HEALING      8)

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH   8)
(define SLIMINESS       5)

(define V-SPACER (rectangle 0 10 'solid 'white))
(define H-SPACER (rectangle 10 0 'solid 'white))

(define MONSTER# 12)
(define PER-ROW  4)

(define ORC          (bitmap "orc.png"))
(define HYDRA        (bitmap "hydra.png"))
(define SLIME        (bitmap "slime.bmp"))
(define BRIGAND      (bitmap "brigand.bmp"))
(define PLAYER-IMAGE (bitmap "player.bmp"))

(define PIC-LIST (list ORC HYDRA SLIME BRIGAND))
(define w (apply max (map image-width PIC-LIST)))
(define h (apply max (map image-height PIC-LIST)))

(define FRAME  (rectangle w h 'outline 'white))
(define TARGET (circle (- (/ w 2) 2) 'outline 'blue))

(define ORC-IMAGE     (overlay ORC     FRAME))
(define HYDRA-IMAGE   (overlay HYDRA   FRAME))
(define SLIME-IMAGE   (overlay SLIME   FRAME))
(define BRIGAND-IMAGE (overlay BRIGAND FRAME))

(define HEALTH-DAMAGE   -2)
(define AGILITY-DAMAGE  -3)
(define STRENGTH-DAMAGE -4)

(define STRENGTH  "strength")
(define AGILITY   "agility")
(define HEALTH    "health")
(define LOSE      "YOU LOSE")
(define WIN       "YOU WIN")
(define DEAD      "DEAD")
(define REMAINING "Remaining attacks ")
(define INSTRUCTIONS-2 "Select a monster using the arrow keys")
(define INSTRUCTIONS-1
  "Press S to stab a monster | Press F to Flail wildly | Press H to Heal")

(define HEALTH-BAR-HEIGHT 12)
(define HEALTH-BAR-WIDTH  50)

(define AGILITY-COLOR  "blue")
(define HEALTH-COLOR   "crimson")
(define STRENGTH-COLOR "forest green")
(define MONSTER-COLOR  "crimson")
(define MESSAGE-COLOR  "black")
(define ATTACK-COLOR   "crimson")

(define HEALTH-SIZE (- HEALTH-BAR-HEIGHT 4))
(define DEAD-TEXT-SIZE (- HEALTH-BAR-HEIGHT 2))
(define INSTRUCTION-TEXT-SIZE 16)
(define MESSAGES-SIZE 40)

(define INSTRUCTION-TEXT
  (above (text INSTRUCTIONS-2 (- INSTRUCTION-TEXT-SIZE 2) "blue")
         (text INSTRUCTIONS-1 (- INSTRUCTION-TEXT-SIZE 4) "blue")))

(define DEAD-TEXT (text DEAD DEAD-TEXT-SIZE "crimson"))

;;; Structs

(struct orc-world (player lom attack# target) #:transparent #:mutable)

(struct player (health agility strength) #:transparent #:mutable)

(struct monster (image [health #:mutable]) #:transparent)
(struct orc     monster (club)             #:transparent)
(struct hydra   monster ()                 #:transparent)
(struct slime   monster (sliminess)        #:transparent)
(struct brigand monster ()                 #:transparent)

;;; GUI

(define (start)
  (big-bang (initialize-orc-world)
            (on-key    player-acts-on-monsters)
            (on-draw   render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))

(define (arrange lom)
  (cond [(empty? lom) empty-image]
        [else (define row-image (apply beside (take lom PER-ROW)))
              (above row-image (arrange (drop lom PER-ROW)))]))

(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

(define (initialize-monsters)
  (build-list MONSTER#
              (lambda (_)
                (define health (random+ MONSTER-HEALTH0))
                (case (random 4)
                  [(0) (orc     ORC-IMAGE     health (random+ CLUB-STRENGTH))]
                  [(1) (hydra   HYDRA-IMAGE   health)]
                  [(2) (slime   SLIME-IMAGE   health (random+ SLIMINESS))]
                  [(3) (brigand BRIGAND-IMAGE health)]))))

(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (instructions w)
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append REMAINING na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))

  (above txt INSTRUCTION-TEXT))

(define (message str)
  (text str MESSAGES-SIZE MESSAGE-COLOR))

(define (player-acts-on-monsters w k)
  (unless (zero? (orc-world-attack# w))

    (case (string->symbol k)
      [(a)     (stab     w)]
      [(h)     (heal     w)]
      [(f)     (flail    w)]
      [(e)     (end-turn w)]
      [(n)     (initialize-orc-world)]
      [(right) (move-target w +1)]
      [(left)  (move-target w -1)]
      [(down)  (move-target w (+ PER-ROW))]
      [(up)    (move-target w (- PER-ROW))]))

  (give-monster-turn-if-attack#=0 w)

  w)

(define (render-monsters lom with-target)
  (define target (if (number? with-target) (list-ref lom with-target) 'nope))
  (define (render-one-monster m)
    (define image
      (overlay (if (eq? m target) TARGET empty-image)
               (monster-image m)))
    (define health (monster-health m))
    (define health-bar
      (if (zero? health)
          (overlay DEAD-TEXT (status-bar 0 1 'white ""))
          (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
    (above health-bar image))

  (arrange (map render-one-monster lom)))

(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

(define (render-orc-world w t additional-text)
  (define i-player  (render-player (orc-world-player w)))
  (define i-monster (render-monsters (orc-world-lom w) t))

  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER
                 (above i-monster
                        V-SPACER V-SPACER V-SPACER
                        additional-text)
                 H-SPACER)
         V-SPACER))

(define (render-player p)
  (define s (player-strength p))
  (define a (player-agility  p))
  (define h (player-health   p))

  (above/align 'left
               (status-bar s MAX-STRENGTH STRENGTH-COLOR STRENGTH)
               V-SPACER
               (status-bar a MAX-AGILITY  AGILITY-COLOR  AGILITY)
               V-SPACER
               (status-bar h MAX-HEALTH   HEALTH-COLOR   HEALTH)
               V-SPACER
               V-SPACER
               V-SPACER
               PLAYER-IMAGE))

(define (render-the-end w)
  (render-orc-world w false (message (if (lose? w) LOSE WIN))))
(define (status-bar v-current v-max color label)
  (define w   (* (/ v-current v-max) HEALTH-BAR-WIDTH))
  (define f   (rectangle w                HEALTH-BAR-HEIGHT 'solid   color))
  (define b   (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT 'outline color))

  (beside (overlay/align 'left 'top f b)
          H-SPACER
          (text label HEALTH-SIZE color)))

;;; Actions

(define (all-monsters-attack-player player lom)
  (define (one-monster-attacks-player m)
    (let* ((name (string-append (symbol->string (object-name m)) "-attack"))
           (attack (eval (string->symbol name) my-namespace)))
      (attack m player)))
  (define live-monsters (filter monster-alive? lom))
  (for-each one-monster-attacks-player live-monsters))

(define (flail w)
  (define (current-target w)
    (list-ref (orc-world-lom w) (orc-world-target w)))
  (define target (current-target w))
  (define alive (filter monster-alive? (orc-world-lom w)))
  (define pick# (min (random-quotient (player-strength (orc-world-player w))
                                      FLAIL-DAMAGE)
                     (length alive)))
  (define getem (cons target (take alive pick#)))

  (decrease-attack# w)
  (for-each (lambda (m) (damage-monster m 1)) getem))

(define (give-monster-turn-if-attack#=0 w)
  (when (zero? (orc-world-attack# w))
    (define player (orc-world-player w))
    (all-monsters-attack-player player (orc-world-lom w))
    (set-orc-world-attack#! w (random-number-of-attacks player))))

(define (heal w)
  (decrease-attack# w)
  (player-health+ (orc-world-player w) HEALING))

(define (move-target w delta)
  (define new (+ (orc-world-target w) delta))
  (set-orc-world-target! w (modulo new MONSTER#)))

(define (stab w)
  (define target (list-ref (orc-world-lom w) (orc-world-target w)))
  (define damage (random-quotient (player-strength (orc-world-player w))
                                  STAB-DAMAGE))

  (decrease-attack# w)
  (damage-monster target damage))

;;; Player

(define (player-dead? p)
  (or (zero? (player-health   p))
      (zero? (player-agility  p))
      (zero? (player-strength p))))

(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

(define player-agility+
  (player-update! set-player-agility!  player-agility  MAX-AGILITY))

(define player-health+
  (player-update! set-player-health!   player-health   MAX-HEALTH))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

;;; Monsters

(define (brigand-attack m p)
  (case (random 3)
    [(0) (player-health+   p HEALTH-DAMAGE)]
    [(1) (player-agility+  p AGILITY-DAMAGE)]
    [(2) (player-strength+ p STRENGTH-DAMAGE)]))

(define (damage-monster m delta)
  (set-monster-health! m (interval- (monster-health m) delta)))

(define (hydra-attack m p)
  (player-health+ p (random- (monster-health m))))

(define (monster-alive? m)
  (> (monster-health m) 0))

(define (orc-attack m p)
  (player-health+ p (random- (orc-club m)))  )

(define (slime-attack m p)
  (player-health+ p -1)
  (player-agility+ p (random- (slime-sliminess m))))

;;; Misc Functions

(define (all-dead? lom)
  (not (ormap monster-alive? lom)))

(define (decrease-attack# w)
  (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

(define (end-turn w)
  (set-orc-world-attack#! w 0))

(define (interval+ n m (max-value 100))
  (interval- n (- m) max-value))

(define (interval- n m (max-value 100))
  (min (max 0 (- n m)) max-value))

(define (lose? w)
  (player-dead? (orc-world-player w)))

(define (random+ n)
  (add1 (random n)))

(define (random- n)
  (- (add1 (random n))))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (win? w)
  (all-dead? (orc-world-lom w)))

;;; Main

(module+ main
  (start))
