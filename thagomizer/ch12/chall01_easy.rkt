#lang racket
(require 2htdp/image (except-in 2htdp/universe left right))

;; -----------------------------------------------------------------
;; Structs
(struct dice-world (src board gt) #:transparent)
(struct move (action gt) #:transparent)
(struct territory (index player dice x y) #:transparent)

(define-values (game game? game-board game-player game-moves)
  (let ()
    (struct game (board player delayed-moves) #:transparent)
    (values game
            game?
            game-board
            game-player
            (lambda (g) (force (game-delayed-moves g))))))

;; Territory helpers
(define (territory-set-dice t d)
  (territory (territory-index t) (territory-player t) d (territory-x t) (territory-y t)))

(define (territory-set-player t p)
  (territory (territory-index t) p (territory-dice t) (territory-x t) (territory-y t)))

(define (territory-for-index index board)
  (list-ref board index))

;; -----------------------------------------------------------------
;; Constants

(define PLAYER# 2)
(define DICE# 3)
(define BOARD 5)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 50)

;; AI
(define AI-DEPTH 4)
(define AI 1)

;; Rendering constants
  ;; Territories
(define DICE-OFFSET 6)
(define SIDE 75)
(define OFFSET0 (* 2 SIDE))
(define ROTATION 30)
(define HEX 6)
(define (hexagon color)
  (rotate ROTATION (regular-polygon SIDE HEX "solid" color)))
(define X-OFFSET (image-width (hexagon "black")))
(define Y-OFFSET (* (image-height (hexagon "black")) 3/4))

  ;; Dice and players
(define COLORS
  (list (make-color 255 0 0 100)
        (make-color 0 255 0 100)
        (make-color 0 0 255 100)))
(define FOCUS (rotate ROTATION (regular-polygon SIDE 6 "outline" "black")))
(define D1 (bitmap "graphics/dice1.png"))
(define D2 (bitmap "graphics/dice2.png"))
(define D3 (bitmap "graphics/dice3.png"))
(define D4 (bitmap "graphics/dice4.png"))
(define IMG-LIST (list D1 D2 D3 D4))

  ;; Instructions
(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define INSTRUCT
  "← and → to move among territories, <enter> to mark, <d> to unmark, and <p> to pass")
(define AI-TURN "It's the Mighty AI's turn")
(define YOUR-TURN "It's your turn")
(define INFO-X-OFFSET 100)
(define INFO-Y-OFFSET 50)
(define INSTRUCTIONS (text INSTRUCT TEXT-SIZE TEXT-COLOR))
(define WIDTH (+ (image-width INSTRUCTIONS) 50))
(define HEIGHT 600)
(define (PLAIN)
  (define iw (image-width INSTRUCTIONS))
  (define bw (* SIDE 2 BOARD))
  (set! WIDTH  (+ (max iw bw) 50))
  (set! HEIGHT (+ (* SIDE 2 BOARD) 50))
  (empty-scene WIDTH HEIGHT))
(define (ISCENE)
  (define mt (PLAIN))
  (when (or (> (image-width mt) 1280) (> (image-height mt) 800))
    (error 'scene "it is impossible to draw a ~s x ~s game scene for a 1280 x 800 laptop screen" (image-width mt) (image-height mt)))
  (place-image INSTRUCTIONS (* .5 WIDTH) (* .9 HEIGHT) mt))


;; ------------------------------------------------------------------
;; Functions


;; Main entry point
(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            (on-key interact-with-board)
            (on-draw draw-dice-world)
            (stop-when no-more-moves-in-world? draw-end-of-dice-world)))


(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world)
      (create-world-of-dice-and-doom) new-world))

(define (interact-with-board w k)
  (cond [(key=? "left" k)
         (refocus-board w left)]
        [(key=? "right" k)
         (refocus-board w right)]
        [(key=? "p" k)
         (pass w)]
        [(key=? "\r" k)
         (mark w)]
        [(key=? "d" k)
         (unmark w)]
        [else w]))

;; Rendering
(define (draw-dice-world w)
  (add-player-info
   (game-player (dice-world-gt w))
   (add-board-to-scene w (ISCENE))))

(define (no-more-moves-in-world? w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define player (game-player tree))
  (or (no-more-moves? tree)
      (for/and ((t board))
        (= (territory-player t) player))))

(define (draw-end-of-dice-world w)
  (define board (dice-world-board w))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene w (PLAIN)))
  (overlay message background))

(define (won board)
  (define-values (best-score w) (winners board))
  (if (cons? (rest w)) "It's a tie." "You won."))


;; Making a board
(define (territory-build)
  (for/list ([n (in-range GRID)])
    (territory n (modulo n PLAYER#) (dice) (get-x n) (get-y n))))

(define (dice)
  (add1 (random DICE#)))

(define (get-x n)
  (+ OFFSET0
     (if (odd? (get-row n)) 0 (/ X-OFFSET 2))
     (* X-OFFSET (modulo n BOARD))))

(define (get-y n)
  (+ OFFSET0 (* Y-OFFSET (get-row n))))

;; Building the game tree
(define (game-tree board player dice)
  ;; create tree of attacks from this position; add passing move
  (define (attacks board)
    (for*/list ([src board]
                [dst (neighbors (territory-index src))]
                #:when (attackable? board player src dst))
      (define from (territory-index src))
      (define dst-territory (territory-for-index dst board))
      (define attack-count (territory-dice src))
      (define defend-count (territory-dice dst-territory))
      (define newb (execute board player from dst attack-count defend-count))
      (define attacks-from-newb
        (game newb player (delay (cons (passes newb) (attacks newb)))))
      (move (list from dst) attacks-from-newb)))
  ;; create a passing move and the rest of the game tree
  (define (passes board)
    (define-values (new-dice newb) (distribute board player dice))
    (move '() (game-tree newb (switch player) new-dice)))
  ;; -- START: --
  (game board player (delay (attacks board))))

(define (switch player)
  (modulo (add1 player) PLAYER#))

(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice] [new-board '()])
    ([t board])
    (if (and (= (territory-player t) player)
             (< (territory-dice t) DICE#)
             (not (zero? dice)))
        (values (- dice 1) (cons (add-dice-to t) new-board))
        (values dice (cons t new-board)))))

(define (add-dice-to t)
  (territory-set-dice t (add1 (territory-dice t))))

(define (attackable? board player src dst)
  (define dst-t
    (findf (lambda (t) (= (territory-index t) dst)) board))
  (and dst-t
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

(define (adjust-for-win win-idx lose-idx winner dice-count board)
  (for/list ([t board])
    (define idx (territory-index t))
    (cond [(= idx win-idx) (territory-set-dice t 1)]
          [(= idx lose-idx)
           (define s (territory-set-dice t (- dice-count 1)))
           (territory-set-player s winner)]
          [else t])))

(define (execute board attacker src dst attack-count defend-count)
  (define attack-sum (sum-n-dice attack-count))
  (define defend-sum (sum-n-dice defend-count))
  (define defender (other-player attacker))
  (cond [(> attack-sum defend-sum)
         (adjust-for-win src dst attacker attack-count board)]
        [else
         (adjust-for-win dst src defender defend-count board)]))

;; Getting Neighbors
(define (neighbors pos)
  (define top? (< pos BOARD))
  (define bottom? (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right? (zero? (modulo (add1 pos) BOARD)))
  (define left? (zero? (modulo pos BOARD)))
  (if even-row?
      (even-row pos top? bottom? right? left?)
      (odd-row pos top? bottom? right? left)))

(define (even-row pos top? bottom? right? left?)
  (append (add top?                (- pos BOARD))
          (add bottom?             (+ pos BOARD))
          (add (or top? right?)    (add1 (- pos BOARD)))
          (add (or bottom? right?) (add1 (+ pos BOARD)))
          (add right? (add1 pos))
          (add left? (sub1 pos))))

(define (odd-row pos top? bottom? right? left?)
  (append (add top?               (- pos BOARD))
          (add bottom?            (+ pos BOARD))
          (add (or top? left?)    (sub1 (- pos BOARD)))
          (add (or bottom? left?) (sub1 (+ pos BOARD)))
          (add right?             (add1 pos))
          (add left?              (sub1 pos))))

(define (add b x)
  (if b empty (list x)))


;; Territory focusing and marking
(define (refocus-board w direction)
  (define source (dice-world-src w))
  (define board (dice-world-board w))
  (define tree (dice-world-gt w))
  (define player (game-player tree))
  (define (owner? tid)
    (if source (not (= tid player)) (= tid player)))
  (define new-board (rotate-until owner? board direction))
  (dice-world source new-board tree))

(define (rotate-until owned-by board rotate)
  (define next-list (rotate board))
  (if (owned-by (territory-player (first next-list)))
      next-list
      (rotate-until owned-by next-list rotate)))

(define (left l)
  (append (rest l) (list (first l))))

(define (right l)
  (reverse (left (reverse l))))

;; Handling moves
;; (define (pass w)
;;   (define m (find-move (game-moves (dice-world-gt w)) '()))
;;   (cond [(not m) w]
;;         [else (dice-world #f (game-board m) m)]))
(define (pass w)
  (define m (find-move (game-moves (dice-world-gt w)) '()))
  (cond [(false? m) w]
        [(or (no-more-moves? m) (not (= (game-player m) AI)))
         (dice-world #F (game-board m) m)]
        [else
         (define ai (the-ai-plays m))
         (dice-world #f (game-board ai) ai)]))


(define (unmark w)
  (dice-world #f (dice-world-board w) (dice-world-gt w)))

(define (mark w)
  (define tree   (dice-world-gt w))
  (define board  (dice-world-board w))
  (define source (dice-world-src w))
  (define focus  (territory-index (first board)))
  (if source
      (attacking w source focus)
      (dice-world focus board tree)))

(define (attacking w source target)
  (define feasible (game-moves (dice-world-gt w)))
  (define attack   (list source target))
  (define next     (find-move feasible attack))
  (if next (dice-world #f (game-board next) next) w))

(define (find-move moves a)
  (define m (findf (lambda (m) (equal? (move-action m) a)) moves))
  (and m (move-gt m)))

(define (no-more-moves? g)
  (empty? (game-moves g)))

;; Rendering again
(define (add-player-info player s)
  (define str (whose-turn player))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET s))

(define (add-board-to-scene w s)
  (define board (dice-world-board w))
  (define player (game-player (dice-world-gt w)))
  (define focus? (dice-world-src w))
  (define trtry1 (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s (add-territory trtry1 image s))
  (for/fold ([s base-s])
    ([t (rest board)])
    (add-territory t (draw-territory t) s)))

(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))

(define (add-territory t image scene)
  (place-image image
               (territory-x t)
               (territory-y t)
               scene))

(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (hexagon color) (draw-dice (territory-dice t))))

(define (draw-dice n)
  (define first-dice (get-dice-image 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice]) ([i (- n 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset (* height-dice (+ 0.5 (* i 0.25))))
    (overlay/offset s 0 y-offset dice-image)))

(define (color-chooser n)
  (list-ref COLORS n))

(define (get-dice-image i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))

;; Ending
(define (winners board)
  (for/fold ([best 0] [winners '()]) ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player) (+ result 1) result)))

;; AUX
(define (get-row pos)
  (quotient pos BOARD))

(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

(define (other-player player)
  (if (= player 0) 1 0))

;; Rolling dice
(define (roll-dice)
  (+ 1 (random 6)))

(define (sum-n-dice n)
  (cond [(= n 1) (roll-dice)]
        [else (+ (roll-dice) (sum-n-dice (sub1 n)))]))

;; AI Functions
(define (rate-moves tree depth)
  (for/list ([move (game-moves tree)])
            (list move (rate-position (move-gt move) (- depth 1)))))

(define (rate-position tree depth)
  (cond [(or (= depth 0) (no-more-moves? tree))
         (define-values (best w) (winners (game-board tree)))
         (if (member AI w) (/ 1 (length w)) 0)]
        [else
         (define ratings (rate-moves tree depth))
         (apply (if (= (game-player tree) AI) max min)
                (map second ratings))]))

(define (the-ai-plays tree)
  (define ratings (rate-moves tree AI-DEPTH))
  (define the-move (first (argmax second ratings)))
  (define new-tree (move-gt the-move))
  (if (= (game-player new-tree) AI)
      (the-ai-plays new-tree)
      new-tree))
