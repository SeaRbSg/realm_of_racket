#lang racket

(require 2htdp/image (except-in 2htdp/universe left right))

;;; 10.5: Structs

(struct dice-world (src board gt)          #:transparent)
(struct territory  (index player dice x y) #:transparent)
(struct game       (board player moves)    #:transparent)
(struct move       (action gt)             #:transparent)

;;; Constants

(define PLAYER# 2)
(define DICE# 3)
(define BOARD 2)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)
;; (define AI-DEPTH 4)         ; The depth at which to limit the gametree
(define AI 1)

;; graphical constants: territories
;; (define DICE-OFFSET 6)
(define SIDE 75)
(define OFFSET0 (* 2 SIDE))
(define ROTATION 30)
(define HEX 6)
(define (hexagon color)
  (rotate ROTATION (regular-polygon SIDE HEX "solid" color)))
(define X-OFFSET (image-width (hexagon "black")))
(define Y-OFFSET (* (image-height (hexagon "black")) 3/4))

;; graphical constants
(define COLORS
  (list (make-color 255 0 0 100)
        (make-color 0 255 0 100)
        (make-color 0 0 255 100)))
(define FOCUS (rotate ROTATION (regular-polygon SIDE 6 "outline" "black")))
(define D1 (bitmap "dice1.png"))
(define D2 (bitmap "dice2.png"))
(define D3 (bitmap "dice3.png"))
(define D4 (bitmap "dice4.png"))
(define IMG-LIST (list D1 D2 D3 D4))

(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define INSTRUCT "← and → to move among territories, <m> to mark, <d> to unmark, and <p> to pass")
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

;;; 10.6: GUI: Roll the Dice

(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            (on-key    interact-with-board)
            (on-draw   draw-dice-world)
            (stop-when no-more-moves-in-world? draw-end-of-dice-world)))

(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world)
      (create-world-of-dice-and-doom)
      new-world))

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

;;; 10.7: Rendering the World

(define (draw-dice-world w)
  (define (add-player-info p s)
    (define str (whose-turn p))
    (define txt (text str TEXT-SIZE TEXT-COLOR))
    (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET s))

  (add-player-info (game-player (dice-world-gt w))
                   (add-board-to-scene w (ISCENE))))

(define (interact-with-board w k)
  (case (string->symbol k)
    [(left)  (refocus-board w left)]
    [(right) (refocus-board w right)]
    [(p)     (pass w)]
    [(m)     (mark w)]
    [(d)     (unmark w)]
    [else    w]))

(define (add-board-to-scene w s)
  (define board (dice-world-board w))
  (define player (game-player (dice-world-gt w)))
  (define focus? (dice-world-src w))
  (define trtry1 (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s (add-territory trtry1 image s))

  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))

(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))

;; TODO
;; (define (draw-focus marked? p-in-focus p t-image)
;;   (if (xor marked? (= p-in-focus p))
;;       (overlay FOCUS t-image)
;;       t-image))

(define (add-territory t image scene)
  (place-image image (territory-x t) (territory-y t) scene))

(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (hexagon color) (draw-dice (territory-dice t))))

(define (color-chooser n)
  (list-ref COLORS n))

(define (draw-dice n)
  (define first-dice  (get-dice-image 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice]) ([i (- n 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset (* height-dice (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

(define (get-dice-img i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))

;;; 10.8 Input Handling

(define (refocus-board w direction)
  (define source (dice-world-src w))
  (define board  (dice-world-board w))
  (define tree   (dice-world-gt w))
  (define player (game-player tree))
  (define (owner? tid) (if source (not (= tid player)) (= tid player)))
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

(define (pass w)
  (define m (find-move (game-moves (dice-world-gt w)) empty))
  (cond [(not m) w]
        [else (dice-world #f (game-board m) m)]))

(define (find-move moves action)
  (define m (findf (lambda (m) (equal? (move-action m) action)) moves))
  (and m (move-gt m)))

(define (mark w)
  (define source (dice-world-src w))
  (define board  (dice-world-board w))
  (define tree   (dice-world-gt w))
  (define focus  (territory-index (first board)))
  (if source
      (attacking w  source focus)
      (dice-world focus board tree)))

(define (attacking w source target)
  (define feasible (game-moves (dice-world-gt w)))
  (define attack (list source target))
  (define next (find-move feasible attack))
  (if next (dice-world false (game-board next) next) w))

(define (unmark w)
  (define source (dice-world-src w))
  (define board  (dice-world-board w))
  (define tree   (dice-world-gt w))
  (dice-world false board tree))

;;; 10.9: Creating a Game Tree

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

;;; The Game Tree

(define (game-tree board player dice)
  (define (attacks board)
    (for*/list ([src board]
                [dst (neighbors (territory-index src))]
                #:when (attackable? board player src dst))
      (define from (territory-index src))
      (define dice (territory-dice src))
      (define newb (execute board player from dst dice))
      (define more (cons (passes newb) (attacks newb)))
      (move (list from dst) (game newb player more))))
  (define (passes board)
    (define-values (new-dice newb) (distribute board player dice))
    (move empty (game-tree newb (switch player) new-dice)))
  (game board player (attacks board)))

(define (switch player)
  (modulo (add1 player) PLAYER#))

(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice] [new-board empty]) ([t board])
    (if (and (= (territory-player t) player)
             (< (territory-dice t) DICE#)
             (not (zero? dice)))
        (values (- dice 1) (cons (add-dice-to t) new-board))
        (values dice (cons t new-board)))))

(define (add-dice-to t)
  (territory-set-dice t (add1 (territory-dice t))))

;;; Neighbors

(define (add b x)
  (if b empty (list x)))

(define (neighbors pos)
  (define top?      (< pos BOARD))
  (define bottom?   (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right?    (zero? (modulo (add1 pos) BOARD)))
  (define left?     (zero? (modulo pos BOARD)))
  (if even-row?
      (even-row pos top? bottom? right? left?)
      (odd-row  pos top? bottom? right? left?)))

(define (even-row pos top? bottom? right? left?) ;; REFACTOR prev/next
  (append (add (or top? right?)    (add1 (- pos BOARD)))
          (add (or bottom? right?) (add1 (+ pos BOARD)))
          (add top?                (- pos BOARD))
          (add bottom?             (+ pos BOARD))
          (add right?              (add1 pos))
          (add left?               (sub1 pos))))

(define (odd-row pos top? bottom? right? left?) ;; REFACTOR prev/next
  (append (add top?               (- pos BOARD))
          (add bottom?            (+ pos BOARD))
          (add (or top? left?)    (sub1 (- pos BOARD)))
          (add (or bottom? left?) (sub1 (+ pos BOARD)))
          (add right?             (add1 pos))
          (add left?              (sub1 pos))))

(define (attackable? board player src dst)
  (define dst-t (findf (lambda (t) (= (territory-index t) dst)) board))
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

(define (won board)
  (define-values (best-score w) (winners board))
  (if (cons? (rest w)) "It's a tie." "You won."))

(define (winners board)
  (for/fold ([best 0] [winners empty]) ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond [(> p-score best) (values p-score (list p))]
          [(< p-score best) (values best winners)]
          [(= p-score best) (values best (cons p winners))])))

(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player) (+ result 1) result)))

;;; Misc

(define (no-more-moves? g)
  (empty? (game-moves g)))

;; TODO: THIS REQUIRES A DIFFERENT DEFINITION FOR PLAIN CHAPTER 10.
(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

(define (get-dice-image i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))

(define (territory-set-dice t d)
  (territory (territory-index t)
             (territory-player t)
             d
             (territory-x t)
             (territory-y t)))

(define (territory-set-player t p)
  (territory (territory-index t)
             p
             (territory-dice t)
             (territory-x t)
             (territory-y t)))

;;; Main

(module+ main
  (roll-the-dice))

(module+ test

)
