#lang racket
(require 2htdp/image (except-in 2htdp/universe left right))
;; Constants

; initalization constants
(define PLAYER# 2)
(define DICE# 3)
(define BOARD 2)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)
; The depth at which to limit the gametree
(define AI-DEPTH 4)
(define AI 1)

; graphical constants: territories
(define DICE-OFFSET 6)
(define SIDE 75)
(define OFFSET0 (* 2 SIDE))
(define ROTATION 30)
(define HEX 6)
(define (hexagon color)
  (rotate ROTATION (regular-polygon SIDE HEX "solid" color)))
(define X-OFFSET (image-width (hexagon "black")))
(define Y-OFFSET (* (image-height (hexagon "black")) 3/4))

; graphical constants
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

(struct dice-world (src board gametree)) ;; nodes on the game tree are 'game' structs linked by moves
(struct territory (index player dice x y))
(struct game (board player moves))  ;; board is a list of territories
(struct move (action gametree)) ;; action is a list of two numbers, the src and target
 
(define b0 (list (territory 0 0 2 'x 'y) (territory 1 1 1 'a 'b)))
(define b1 (list (territory 0 0 1 'x 'y) (territory 1 0 1 'a 'b)))
(define b2 (list (territory 1 0 1 'a 'b) (territory 0 0 1 'x 'y)))

(define gt2 (game b2 1 empty))

(define mv1 (move empty gt2))
(define gt1 (game b1 0 (list mv1)))

(define mv0 (move '(0 1) gt1))
(define gt0 (game b0 0 (list mv0)))

(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            (on-key interact-with-board)
            (on-draw draw-dice-world)
            (stop-when no-more-moves-in-world?
                       draw-end-of-dice-world)))

(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world) 
      (create-world-of-dice-and-doom)
      new-world))

(define (no-more-moves-in-world? w)
  (define tree (dice-world-gametree w))
  (define board (dice-world-board w))
  (define player (game-player tree))
  (or (no-more-moves? tree)
      (for/and ((t board)) (= (territory-player t) player))))

(define (no-more-moves? g)
  (empty? (game-moves g)))

(define (interact-with-board world k)
  (cond [(key=? "left" k)
	 (refocus-board world left)]
	[(key=? "right" k)
	 (refocus-board world right)]
	[(key=? "p" k)
	 (pass world)]
	[(key=? "\r" k)
	 (mark world)]
	[(key=? "d" k)
	 (unmark world)]
	[else world]))

;; Rendering
(define (draw-end-of-dice-world world)
  (define board (dice-world-board world))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene world (PLAIN)))
  (overlay message background))

(define (draw-dice-world world)
  (add-player-info
    (game-player (dice-world-gametree world))
    (add-board-to-scene world (ISCENE))))

(define (add-player-info player scene)
  (define player-turn-str (whose-turn player))
  (define player-turn-txt (text player-turn-str TEXT-SIZE TEXT-COLOR))
  (place-image player-turn-txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET scene))

(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

(define (add-board-to-scene world scene)
  (define board (dice-world-board world))
  (define player (game-player (dice-world-gametree world)))
  (define focus? (dice-world-src world))
  (define territory1 (first board))
  (define player-focus (territory-player territory1))
  (define territory-image (draw-territory territory1))
  (define image (draw-focus focus? player-focus player territory-image))
  (define base-scene (add-territory territory1 image scene))
  (for/fold ([s base-scene])
	    ([t (rest board)])
	    (add-territory t (draw-territory t) s)))

(define (draw-focus marked? player-in-focus player territory-image)
  (if (or (and (not marked?) (= player-in-focus player))
	  (and marked? (not (= player-in-focus player))))
    (overlay FOCUS territory-image)
    territory-image))

(define (add-territory territory image scene)
  (place-image image (territory-x territory) (territory-y territory) scene))

(define (draw-territory territory)
  (define color (color-chooser (territory-player territory)))
  (overlay (hexagon color) (draw-dice (territory-dice territory))))

(define (color-chooser player#)
  (list-ref COLORS player#))

(define (draw-dice number)
  (define first-dice (get-dice-image 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice])
	    ([i (- number 1)])
	    (define dice-image (get-dice-image (+ i 1)))
	    (define y-offset (* height-dice (+ .5 (* i .25))))
	    (overlay/offset s 0 y-offset dice-image)))

(define (get-dice-image i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))

;; Input Handling

(define (refocus-board world direction)
  (define source (dice-world-src world))
  (define board (dice-world-board world))
  (define tree (dice-world-gametree world))
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

(define (pass world)
  (define move (find-move (game-moves (dice-world-gametree world)) empty))
  (cond [(not move) world]
	[else (dice-world #f (game-board move) move)]))

(define (find-move moves action)
  (define m
    (findf (lambda (m) (equal? (move-action m) action)) moves))
  (and m (move-gametree m)))

(define (mark world)
  (define tree (dice-world-gametree world))
  (define board (dice-world-board world))
  (define source (dice-world-src world))
  (define focus (territory-index (first board)))
  (if source
    (attacking world source focus)
    (dice-world focus board tree)))

(define (attacking world source target)
  (define feasable (game-moves (dice-world-gametree world)))
  (define attack (list source target))
  (define next (find-move feasable attack))
  (if next (dice-world #f (game-board next) next) world))

(define (unmark world)
  (dice-world #f (dice-world-board world) (dice-world-gametree world)))

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

(define (get-row pos)
  (quotient pos BOARD))

;; The game tree

(define (game-tree board player dice)
  (define (attacks board)
    (for*/list ([src board]
		[dst (neighbors (territory-index src))]
		#:when (attackable? board player src dst))
	       (define from (territory-index src))
	       (define dice (territory-dice src))
	       (define newboard (execute board player from dst dice))
	       (define moremoves (cons (passes newboard) (attacks newboard)))
	       (move (list from dst) (game newboard player moremoves))))
  (define (passes board)
    (define-values (new-dice newboard) (distribute board player dice))
    (move '() (game-tree newboard (switch player) new-dice)))
  (game board player (attacks board)))

(define (switch player)
  (modulo (+ player 1) PLAYER#))

(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice] [new-board '()])
	    ([t board])
	    (if (and (= (territory-player t) player)
		     (< (territory-dice t) DICE#)
		     (not (zero? dice)))
	      (values (- dice 1) (cons (add-dice-to t) new-board))
	      (values dice (cons t new-board)))))

(define (add-dice-to territory)
  (territory-set-dice territory (add1 (territory-dice territory))))

(define (territory-set-dice t d)
  (territory (territory-index t) (territory-player t) d (territory-x t) (territory-y t)))

(define (territory-set-player t p)
  (territory (territory-index t) p (territory-dice t) (territory-x t) (territory-y t)))

(define (add b x)
  (if b empty (list x)))

(define (neighbors pos)  
  (define top? (< pos BOARD))
  (define bottom? (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right? (zero? (modulo (add1 pos) BOARD)))
  (define left? (zero? (modulo pos BOARD)))
  (if even-row? 
    (even-row pos top? bottom? right? left?)
    (odd-row  pos top? bottom? right? left?)))

(define (even-row pos top? bottom? right? left?)
  (append (add (or top? right?) (add1 (- pos BOARD)))
	  (add (or bottom? right?) (add1 (+ pos BOARD)))
          (add top? (- pos BOARD))
          (add bottom? (+ pos BOARD))
          (add right? (add1 pos))
          (add left? (sub1 pos))))

(define (odd-row pos top? bottom? right? left?)
  (append (add top? (- pos BOARD))
          (add bottom? (+ pos BOARD))
          (add (or top? left?) (sub1 (- pos BOARD)))
          (add (or bottom? left?) (sub1 (+ pos BOARD)))
          (add right? (add1 pos))
          (add left? (sub1 pos))))

(define (attackable? board player src dst)
  (define dst-t 
    (findf (lambda (t) (= (territory-index t) dst)) board))
  (and dst-t
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

(define (execute board player src dst dice)
  (for/list ([t board])
	    (define idx (territory-index t))
	    (cond [(= idx src) (territory-set-dice t 1)]
		  [(= idx dst) 
		   (define s (territory-set-dice t (- dice 1)))
		   (territory-set-player s player)]
		  [else t])))

;; The End Game

(define (won board)
  (define-values (best-score w) (winners board))
  (if (cons? (rest w)) "It's a tie." "You won."))

(define (winners board)
  (for/fold ([best 0][winners '()]) ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player) (+ result 1) result)))
