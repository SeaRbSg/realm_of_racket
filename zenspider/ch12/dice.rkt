#lang racket

(random-seed 42)

(require 2htdp/image (except-in 2htdp/universe left right))

;;; 10.5: Structs

(struct dice-world (src board gt moves)    #:transparent #:omit-define-syntaxes #:extra-constructor-name -dice-world)
(struct territory  (index player dice)     #:transparent #:omit-define-syntaxes #:extra-constructor-name -territory)
(struct move       (action gt)             #:transparent)

(define (dice-world src board gt [moves #f])
  (-dice-world src board gt moves))

(define (territory index player dice [a #f] [b #f])
  (-territory index player dice))

;; This is fucking _smart_. I like it:

(define-values (game game? game-board game-player game-moves)
  (let ()
    (struct game (board player delayed-moves) #:transparent)
    (values game
            game?
            game-board
            game-player
            (lambda (g) (force (game-delayed-moves g))))))

;;; Constants

(define PLAYER# 2)
(define DICE# 3)
(define SIZE-DIE 6)
(define BOARD 3)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)
(define AI-DEPTH 4)
(define AI 1)

;; graphical constants: territories

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
(define FOCUS (rotate ROTATION (regular-polygon SIDE HEX "outline" "black")))
(define IMG-LIST (list
                  (bitmap "../ch10/dice1.png")
                  (bitmap "../ch10/dice2.png")
                  (bitmap "../ch10/dice3.png")
                  (bitmap "../ch10/dice4.png")))

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
  (when (or (> (image-width mt) 1280)
            (> (image-height mt) 800))
        (error 'scene
               "it is impossible to draw a ~s x ~s game scene for a 1280 x 800 laptop screen"
               (image-width mt)
               (image-height mt)))
  (place-image INSTRUCTIONS (* .5 WIDTH) (* .9 HEIGHT) mt))

;;; 10.6: GUI: Roll the Dice

(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            #;(on-tick   run-through-moves)
            (on-key    interact-with-board)
            (to-draw   draw-dice-world)
            (stop-when no-more-moves-in-world? draw-end-of-dice-world)))

#;(define (run-through-moves w)
  (if (ai-has-moves? w)
      (let* ((moves (dice-world-moves w))
             (move  (first moves)))
        (dice-world false (game-board (move-gt move)) (move-gt move) (rest moves)))
      w))

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
  (when (ai-has-moves? w) (raise "woot"))
  (case (string->symbol k)
    [(left)  (refocus-board w right)]
    [(right) (refocus-board w left)]
    [(escape) (stop-with w)]
    [(p)     (pass w)]
    [(m)     (mark w)]
    [(d)     (unmark w)]
    [else    w]))

(define (add-board-to-scene w s)
  (define board   (dice-world-board w))
  (define player  (game-player (dice-world-gt w)))
  (define focus?  (dice-world-src w))
  (define trtry1  (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image   (draw-focus focus? p-focus player t-image))
  (define base-s  (add-territory trtry1 image s))

  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))

(define (draw-focus marked? p-in-focus p t-image)
  (if (xor marked? (= p-in-focus p))
      (overlay FOCUS t-image)
      t-image))

(define (add-territory t image scene)
  (place-image image
               (get-x (territory-index t))
               (get-y (territory-index t))
               scene))

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
  (cond [(false? m) w]
        [(or (no-more-moves? m) (not (ai-turn? m)))
         (dice-world false (game-board m) m)]
        [else
         (define ai (the-ai-plays m))
         #;(dice-world false (game-board m) m ai) ; TODO: not sure if (game-board m) is right
         (dice-world false (game-board ai) ai)]))

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
  (define board  (dice-world-board w))
  (define tree   (dice-world-gt w))
  (dice-world false board tree))

;;; 10.9: Creating a Game Tree

(define (territory-build)
  (for/list ([n (in-range GRID)])
    (territory n (modulo n PLAYER#) (dice))))

(define (roll rolls sides)
  (for/sum ([n rolls]) (add1 (random sides))))

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

(define (game-tree board s-player dice)
  (define (attacks board)
    (for*/list ([src board]
                [d-idx (neighbors (territory-index src))]
                #:when (attackable? board s-player src d-idx)) ; FIX dumb mix
      (define dst      (list-ref board d-idx))
      (define s-idx    (territory-index src))
      (define s-dice   (territory-dice src))
      (define d-dice   (territory-dice dst))
      (define d-player (territory-player dst))
      (define s-score  (roll s-dice SIZE-DIE))
      (define d-score  (roll d-dice SIZE-DIE))
      (define newb     (if (> s-score d-score)
                           (attack board s-player s-idx d-idx 1 (sub1 s-dice))
                           (attack board d-player s-idx d-idx 1 d-dice)))
      (define gt-attack
        (game newb s-player (delay (cons (passes newb) (attacks newb)))))
      (move (list s-idx d-idx) gt-attack)))
  (define (passes board)
    (define-values (new-dice newb) (distribute board s-player dice))
    (move empty (game-tree newb (switch s-player) new-dice)))
  (game board s-player (delay (attacks board))))

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
  (define row-fn    (if even-row? even-row odd-row))
  (row-fn pos top? bottom? right? left?))

(define (even-row pos top? bottom? right? left?)
  (define prev (- pos BOARD))
  (define next (+ pos BOARD))
  (append (add (or top? right?)    (add1 prev))
          (add (or bottom? right?) (add1 next))
          (add top?                prev)
          (add bottom?             next)
          (add right?              (add1 pos))
          (add left?               (sub1 pos))))

(define (odd-row pos top? bottom? right? left?)
  (define prev (- pos BOARD))
  (define next (+ pos BOARD))
  (append (add top?               prev)
          (add bottom?            next)
          (add (or top? left?)    (sub1 prev))
          (add (or bottom? left?) (sub1 next))
          (add right?             (add1 pos))
          (add left?              (sub1 pos))))

(define (attackable? board player s-ter d-idx)
  (define dst-t (findf (lambda (t) (= (territory-index t) d-idx)) board))
  (and dst-t
       (= (territory-player s-ter) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice s-ter) (territory-dice dst-t))))

(define (attack board player src dst sdice ddice)
  (for/list ([t board])
    (define idx (territory-index t))
    (cond [(= idx src) (territory-set-dice t sdice)]
          [(= idx dst)
           (define s (territory-set-dice t ddice))
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

;;; 12.3: AI

(define (ai-has-moves? w)
  (let ((moves (dice-world-moves w)))
    (and moves (not (empty? moves)))))

(define (rate-moves tree depth)
  (for/list ([move (game-moves tree)])
    (list move (rate-positions (move-gt move) (- depth 1)))))

(define (rate-positions tree depth)
  (cond [(or (zero? depth) (no-more-moves? tree))
         (define-values (best w) (winners (game-board tree)))
         (if (member AI w)
             (/ 1 (length w))
             0)]
        [else
         (define ratings (rate-moves tree depth))
         (apply (if (ai-turn? tree) max min)
                (map second ratings))]))

(define (new-the-ai-plays tree)
  (define ratings  (rate-moves tree AI-DEPTH))
  (define the-move (first (argmax second ratings)))
  (define new-tree (move-gt the-move))
  (if (ai-turn? new-tree)
      (cons the-move (the-ai-plays new-tree))
      empty))

(define (the-ai-plays tree)
  (define ratings  (rate-moves tree AI-DEPTH))
  (define the-move (first (argmax second ratings)))
  (define new-tree (move-gt the-move))
  (if (ai-turn? new-tree)
      (the-ai-plays new-tree)
      new-tree))

;;; Misc

(define (ai-turn? game)
  (= (game-player game) AI))

(define (no-more-moves? g)
  (empty? (game-moves g)))

(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

(define (get-dice-image i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))

(define (territory-set-dice t d)
  (territory (territory-index t)
             (territory-player t)
             d))

(define (territory-set-player t p)
  (territory (territory-index t)
             p
             (territory-dice t)))

;;; Main

(module+ test
  (require rackunit)

  (define (rate-position tree depth) ; HACK
    (rate-positions tree depth))

  (define (execute board player src dst sdice [ddice 0]) ; HACK
    (attack board player src dst sdice ddice))

  ;; sample game tree for BOOK

  (define b1
    (list (territory 1 0 1 'a 'b)
          (territory 0 0 1 'x 'y)))

  (define b1-alternative
    (list (territory 0 0 1 'x 'y)
          (territory 1 0 1 'a 'b)))

  (define b3
    (list (territory 0 0 2 'x 'y)
          (territory 1 1 1 'a 'b)))

  (define gt1 (game b1 1 (delay '())))

  (define mv2 (move '() gt1))

  (define gt2 (game b1-alternative 0 (delay (list mv2))))

  (define mv3 (move '(0 1) gt2))

  (define gt3 (game b3 0 (delay (list mv3))))

  (define (mapper vals)
    (map (lambda (v) (apply territory v)) vals))

  (check-equal? (mapper '((0 0 1) (1 1 1) (2 0 3) (3 1 1)))
                (list (territory 0 0 1)
                      (territory 1 1 1)
                      (territory 2 0 3)
                      (territory 3 1 1)))

  (random-seed 42)
  #;(check-equal? (create-world-of-dice-and-doom)
                (let ((t1 (mapper '((0 0 1) (1 1 1) (2 0 3) (3 1 1))))
                      (t2 (mapper '((0 0 1) (1 1 1) (2 0 1) (3 0 2))))
                      (t3 (mapper '((3 0 3) (2 0 2) (1 1 1) (0 0 2))))
                      (t4 (mapper '((0 0 1) (1 0 1) (2 0 1) (3 0 1))))
                      (t5 (mapper '((3 0 2) (2 0 2) (1 0 2) (0 0 2)))))
                  (dice-world
                   false
                   t1
                   (game
                    t1
                    0
                    (list (move
                           '(2 3)
                           (game
                            t2
                            0
                            (list (move empty (game t3 1 empty))
                                  (move '(3 1)
                                        (game
                                         t4
                                         0
                                         (list (move empty (game t5 1 empty))))))))))))))

(define (set-grid n)
  (set! BOARD n)
  (set! GRID (* n n)))

(module+ test
  (require "../check-sexp-equal.rkt" #;(for-syntax "../check-sexp-equal.rkt"))
  (require rackunit rackunit/text-ui)

  (define old-size BOARD)
  (set-grid 2)

  ;; (-> any) -> void
  ;; runs the thunk PROP-NUM times
  (define (check-property t)
    (test-begin (for ((i 50)) (t))))

  ;; Properties
  (define (property:starting-world-playable)
    (unless (and (<= BOARD 5) (= PLAYER# 2))
      (error 'starting-world-playable "BOARD-SIZE != 2 or PLAYERS# != 2"))
    (check-false (no-more-moves-in-world? (create-world-of-dice-and-doom))))

  (define (property:dice-in-range)
    (check-true (andmap (λ (b) (>= DICE# (territory-dice b) 1)) (territory-build))
                "dice out of range"))

  (define (property:board-correct-size)
    (check-equal? (length (territory-build)) GRID
                  "board incorrect-size"))

  (define (property:no-pass-on-first-move)
    (define (move-action? m) (equal? (move-action m) '()))
    (check-true (not (memf move-action? (game-moves (game-tree (territory-build) 0 0))))
                "no pass on first move"))

  ;; ---------------------------------------------------------------------------------------------------


  ;; testing game initialization

  (check-equal? (territory-index (first (territory-build))) 0)
  (check-equal? (territory-player (first (territory-build))) 0)
  (check-equal? (territory-index (second (territory-build))) 1)
  (check-equal? (territory-player (second (territory-build))) 1)
  (check-equal? (territory-index (third (territory-build))) 2)
  (check-equal? (territory-player (third (territory-build))) 0)
  (check-equal? (territory-index (fourth (territory-build))) 3)
  (check-equal? (territory-player (fourth (territory-build))) 1)

  (check-property property:starting-world-playable)
  (check-property property:board-correct-size)
  (check-property property:dice-in-range)
  (check-property property:no-pass-on-first-move)

  ;; ---------------------------------------------------------------------------------------------------
  ;; testing territory manipulation

  ;; legal?
  (check-true
   (and (attackable? (list (territory 0 0 2 9 0) (territory 3 1 1 9 0)) 0 (territory 0 0 2 9 0) 3) #t))
  (check-false
   (attackable? (list (territory 0 0 2 9 0) (territory 3 1 1 9 0)) 0 (territory 0 0 2 9 0) 0))
  (check-false
   (attackable? (list (territory 0 0 2 9 0) (territory 5 1 1 9 0)) 1 (territory 0 0 2 9 0) 5))

  ;; get-row
  (check-equal? (get-row 0) 0)
  (check-equal? (get-row 1) 0)
  (check-equal? (get-row 2) 1)
  (check-equal? (get-row 3) 1)
  (check-equal? (get-row 12) 6) ;; checks math. actually invalid on board of size 2
  (check-equal? (get-row 11) 5) ;; checks math. actually invalid on board of size 2
  (check-equal? (get-row 13) 6) ;; checks math. actually invalid on board of size 2
  (check-equal? (get-row 14) 7) ;; checks math. actually invalid on board of size 2

  ;; ---------------------------------------------------------------------------------------------------
  (define board3
    (list (territory 0 1 1 9 0) (territory 1 1 1 8 0) (territory 2 1 3 43.5 5) (territory 3 1 1 6 5)))
  (define b1+0+3
    (list (territory 0 0 2 9 0) (territory 1 1 1 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))
  (define b2+1+2
    (list (territory 0 0 1 9 0) (territory 1 1 3 8 0) (territory 2 0 2 43.5 5) (territory 3 1 2 6 5)))
  (define board6
    (list (territory 0 0 1 9 0) (territory 1 1 2 8 0) (territory 2 0 3 43.5 5) (territory 3 1 2 6 5)))
  (define bard6+
    (list (territory 0 0 1 9 0) (territory 1 1 2 8 0) (territory 2 0 3 43.5 5) (territory 3 1 2 6 5)))

  (define (distribute/list a b c)
    (define-values (x y) (distribute a b c))
    (list x y))

  (define board0
    (list (territory 0 0 1 9 0) (territory 1 1 2 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))
  (define board1
    (list (territory 0 0 1 9 0) (territory 1 1 1 8 0) (territory 2 0 1 43.5 5) (territory 3 1 1 6 5)))
  (define b1+1+2
    (list (territory 0 0 1 9 0) (territory 1 1 2 8 0) (territory 2 0 1 43.5 5) (territory 3 1 2 6 5)))
  (define board2
    (list (territory 0 0 1 9 0) (territory 1 1 1 8 0) (territory 2 0 3 43.5 5) (territory 3 1 1 6 5)))

  (define g-tree1 (game board1 0 (delay '())))
  (define g-tree2 (game-tree board0 0 0))

  ; (define world31 (dice-world #f board1 g-tree1))
  (define world2 (dice-world #f board2 g-tree2))

  ;; testing book tree

  (define (game-tree=? gt1 gt2)
    (and (equal? (game-player gt1) (game-player gt2))
         (equal? (game-board gt1) (game-board gt2))
         (= (length (game-moves gt1)) (length (game-moves gt2)))))

  (check game-tree=?
         (game-tree (list (territory 0 0 2 'x 'y) (territory 1 1 1 'a 'b)) 0 0)
         gt3)

  ;; testing tree generation

  (define (property:attack-location-valid)
    (define moves (game-moves (game-tree (territory-build) 0 0)))
    (check-true (and (for/and ([m moves])
                       (define m1 (move-action m))
                       (member (second m1) (neighbors (first m1))))
                     #t)
                "invalid attack location"))

  (define (property:add-to-territory-always-up-one)
    (define r (random 10000))
    (check-equal? (add-dice-to (territory 0 0 r 0 0))
                  (territory 0 0 (add1 r) 0 0)
                  "add to territory always up one"))

  (define (property:attackable?-does-not-need-neighbores-check)
    (define (check-attackable? gt)
      (for/and ([move (game-moves gt)]
                #:when (not (empty? (move-action move))))
        (define action (move-action move))
        (define gt (move-gt move))
        (and (member (second action) (neighbors (first action)))
             (check-attackable? gt))))

    ;;start
    (define testing-gt (dice-world-gt (create-world-of-dice-and-doom)))
    (check-true (check-attackable? testing-gt) "An attack move between non-neighbores was created")
    )


  ;; game-tree
  (check game-tree=? (game-tree board1 0 0) g-tree1)
  (check game-tree=? (game-tree board3 1 0) (game board3 1 (delay '())))
  (check game-tree=? (game-tree board3 0 0) (game board3 0 (delay '())))
  (check-property property:attackable?-does-not-need-neighbores-check)

  ;; find-move
  (define gt0 (game '() 0 (delay '())))

  (check-false (find-move '() '()))
  (check game-tree=? (find-move (list (move '() gt0)) '()) gt0)
  ;; Attacking-Moves
  (check-property property:attack-location-valid)

  ;; switch-players
  (check-equal? (switch 0) 1)
  (check-equal? (switch 1) 0)

  ;; Add-New-Dice
  (check-equal? (distribute/list (game-board g-tree1) 0 3) (list 1 (reverse b1+0+3)))
  (check-equal? (distribute/list (game-board g-tree1) 1 2) (list 0 (reverse b1+1+2)))
  (check-equal? (distribute/list (game-board g-tree2) 1 2) (list 0 (reverse b2+1+2)))
  (check-equal? (distribute/list board6 0 0) (list 0 (reverse bard6+)))

  ;; add-to-territory
  (check-equal? (add-dice-to (territory 0 1 2 9 0)) (territory 0 1 3 9 0))
  (check-equal? (add-dice-to (territory 0 1 1 9 0)) (territory 0 1 2 9 0))
  (check-equal? (add-dice-to (territory 0 1 5 9 0)) (territory 0 1 6 9 0))
  (check-property property:add-to-territory-always-up-one)

  ;; ---------------------------------------------------------------------------------------------------
  (define board7
    (list (territory 0 0 1 9 0) (territory 1 1 1 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))
  (define board8
    (list (territory 0 1 1 9 0) (territory 1 1 1 8 0) (territory 2 0 3 43.5 5) (territory 3 1 1 6 5)))
  (define board9
    (list (territory 0 0 1 9 0) (territory 1 1 1 8 0) (territory 2 0 2 43.5 5) (territory 3 0 1 6 5)))
  (define board10
    (list (territory 0 0 1 9 0) (territory 1 1 3 8 0) (territory 2 0 2 43.5 5) (territory 3 1 1 6 5)))

  ;; testing attacks

  #;(check-equal?
   (execute board7 0 2 1 2)
   (list (territory 0 0 1 9 0) (territory 1 0 1 8 0) (territory 2 0 1 43.5 5) (territory 3 1 1 6 5)))

  #;(check-sexp-equal?
   (execute board8 0 2 1 3)
   (list (territory 0 1 1 9 0) (territory 1 0 2 8 0) (territory 2 0 1 43.5 5) (territory 3 1 1 6 5)))

  #;(check-sexp-equal?
   (execute board9 0 2 1 2)
   (list (territory 0 0 1 9 0) (territory 1 0 1 8 0) (territory 2 0 1 43.5 5) (territory 3 0 1 6 5)))

  #;(check-sexp-equal?
   (execute board10 1 1 0 3)
   (list (territory 0 1 2 9 0) 
         (territory 1 1 1 8 0) 
         (territory 2 0 2 43.5 5) 
         (territory 3 1 1 6 5)))

  ;; Neighbors
  (check-equal? (neighbors 2) '(0 3))
  (check-equal? (neighbors 0) '(3 2 1))
  (check-equal? (neighbors 1) '(3 0))
  (check-equal? (neighbors 3) '(1 0 2))

  ;; ---------------------------------------------------------------------------------------------------
  (define board20
    (list (territory 0 0 1 9 2) (territory 1 0 1 9 0) (territory 2 2 1 9 0)))
  (define board21
    (list (territory 0 1 1 9 0) (territory 1 1 1 8 0) (territory 2 1 1 43.5 5) (territory 3 1 1 6 5)))

  ;; testing focus manipulation
  ;; interact-with-board
  #;(check-equal?
   (interact-with-board world2 "\r")
   (dice-world (territory-index (car (dice-world-board world2))) (dice-world-board world2) g-tree2))

  (check-equal? (interact-with-board world2 "p") world2)

  ;; refocus-board-action
  (check-equal?
   (refocus-board (dice-world #f (list (territory 0 0 1 9 0) (territory 0 0 1 9 2)) g-tree1) left)
   (dice-world #f (list (territory 0 0 1 9 2) (territory 0 0 1 9 0)) g-tree1))

  (check-equal?
   (refocus-board (dice-world #f (list (territory 0 0 1 9 2) (territory 0 1 1 9 0)) g-tree1) right)
   (dice-world #f (list (territory 0 0 1 9 2) (territory 0 1 1 9 0)) g-tree1))

  (check-equal?
   (refocus-board (dice-world 0 board20 g-tree1) left)
   (dice-world 0 (list (territory 2 2 1 9 0) (territory 0 0 1 9 2) (territory 1 0 1 9 0)) g-tree1))

  (check-equal?
   (refocus-board (dice-world 0 (list (territory 0 0 1 9 2) (territory 0 1 1 9 0)) g-tree1) left)
   (dice-world 0 (list  (territory 0 1 1 9 0) (territory 0 0 1 9 2)) g-tree1))

  (check-equal?
   (refocus-board (dice-world 0 (list(territory 0 0 1 9 2) (territory 0 1 1 9 0)) g-tree1) right)
   (dice-world 0 (list  (territory 0 1 1 9 0) (territory 0 0 1 9 2)) g-tree1))

  ;;unmark
  (check-equal? (unmark (dice-world 1 board21 g-tree1)) (dice-world #f board21 g-tree1))

  (check-equal? (unmark (dice-world 1 (list (territory 0 1 1 9 0) (territory 1 1 1 8 0)) g-tree1))
                (dice-world #f (list (territory 0 1 1 9 0) (territory 1 1 1 8 0)) g-tree1))
  (check-equal? (unmark (dice-world 0 (list (territory 0 1 1 9 0)) g-tree1))
                (dice-world #f (list (territory 0 1 1 9 0)) g-tree1))
  (check-equal? (unmark (dice-world #f (list (territory 0 1 1 9 0)) g-tree1))
                (dice-world #f (list (territory 0 1 1 9 0)) g-tree1))

  ;; ---------------------------------------------------------------------------------------------------
  (define (winners/list w)
    (define-values (a b) (winners w))
    (cons a b))

  ;; testing functions that determine 'winning' and declare the winner

  ;; winners
  (check-equal? (winners/list (list (territory 0 0 1 9 0) (territory 0 0 1 9 1))) (list 2 0))
  (check-equal? (winners/list (list (territory 0 1 1 9 0) (territory 0 0 1 9 1))) (list 1 1 0))

  ;; sum-territory
  (check-equal? (sum-territory (list (territory 0 0 1 9 0) (territory 0 0 1 9 1)) 0) 2)
  (check-equal? (sum-territory (list (territory 0 0 1 9 0) (territory 0 0 1 9 1)) 1) 0)
  (check-equal? (sum-territory (list (territory 0 0 1 9 0) (territory 0 0 1 9 1)) 2) 0)
  (check-equal? (sum-territory (list (territory 0 1 1 9 0) (territory 0 0 1 9 1)) 1) 1)
  (check-equal? (sum-territory (list (territory 0 1 1 9 0) (territory 0 0 1 9 1)) 0) 1)

  ;; ---------------------------------------------------------------------------------------------------
  ;; testing the AI

  (define tree0
    (game-tree (list (territory 0 1 3 0 0)
                     (territory 1 0 2 0 0)
                     (territory 2 0 2 0 0)
                     (territory 3 0 2 0 0))
               1 15))

  (define territory1 (territory 3 0 3 280 262.5))

  (define board31
    (list territory1
          (territory 2 0 3 150 262.5)
          (territory 1 1 2 345 150)
          (territory 0 0 2 215 150)))

  (define world1
    (dice-world #f board31 (game board31 1 (delay '()))))

  ;; testing the AI functions

  ;; MF: one of these two tests should fail!
  (check-true (and (attackable? board31 0 territory1 1) #t))
  (check-true (no-more-moves-in-world? world1))

  (check-equal? (interact-with-board (dice-world 3 '() '()) "d")
                (dice-world #f '() '()))

  (check-equal? (game-board (the-ai-plays tree0))
                (list (territory 3 1 3 0 0)
                      (territory 2 0 2 0 0)
                      (territory 1 0 2 0 0)
                      (territory 0 1 2 0 0)))

  (check-equal? (game-player (the-ai-plays tree0))
                0)

  (check-equal? (game-board (move-gt (first (game-moves tree0))))
                (list (territory 0 1 1 0 0)
                      (territory 1 0 2 0 0)
                      (territory 2 0 2 0 0)
                      (territory 3 1 2 0 0)))

  (check-equal? (game-player (move-gt (first (game-moves tree0))))
                1)

  (check-equal? (rate-position tree0 AI-DEPTH) 1/2)
  (check-equal? (rate-position (move-gt (first (game-moves tree0))) AI-DEPTH)
                1/2)

  (set-grid old-size)
  "all tests run")

(module+ main
  (roll-the-dice))
