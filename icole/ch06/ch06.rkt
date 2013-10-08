#lang racket
(require 2htdp/universe 2htdp/image)

;;CONSTANTS
(define SIZE 500)
(define SEG-SIZE 150)
(define WIDTH-PX  (* SEG-SIZE 10))
(define HEIGHT-PX (* SEG-SIZE 10))
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define EXPIRATION-TIME 150)
(define GOO-IMG (bitmap "goo.gif"))
(define SEG-IMG  (bitmap "body.gif"))
(define HEAD-IMG (bitmap "head.gif"))
(define HEAD-UP-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (flip vertical HEAD-UP-IMG))
(define HEAD-RIGHT-IMG (rotate 90 HEAD-UP-IMG))
(define HEAD-LEFT-IMG (flip-horizontal HEAD-DOWN-IMG))
(define ENDGAME-TEXT-SIZE 50)
(define TICK-RATE 1/3)

;;MAIN STRUCTS

;;Position coordinates
(struct posn (x y))

;;Main struct of pit w/ snake and goos
(struct pit (snake goos))

;;Snake has a direction (up,down,left,right) & segments (posn)
(struct snake (dir segs))

;;Goo has a location (posn) and expiration (# of clock ticks)
(struct goo (loc expire))

;;Creates a new good struct at a random position
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

;;Determines if all ticks are gone on goo
(define (rotten? g)
  (zero? (goo-expire g)))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

;;List eater function that decays eat goo
(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

;;List eater function function that replaces a rotten goo with a new one
(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))

;;Rot and renew all goos
(define (age-goo goos)
  (rot (renew goos)))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

;;List eater function that returns empty when there is one seg left, so it removes that seg
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

;;Assigns a new position for the head based on the direction 
(define (next-head sn)
  (define head (snake-head sn))
  (define dir  (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

;;GROW AND SLITHER
(define (grow sn)
  ;;Create new snake
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

(define (slither sn)
  ;;Create new snake
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

;;Function determines if there is a goo for snake to eat
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

;;On-tick action to check the environment and produce a new world
(define (next-pit w)
  ;;Extract snake a goo from pit
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  ;;Creates function that returns #f or a goo to eat
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

;;KEY-EVENT FUNCTIONS
(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              ;; consists of the head and at least one segment
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-dir the-snake d) (pit-goos w))]))

;;Check if current and desired direction are opposite
(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

;;RENDERING FUNCTIONS
(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene  (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

;;END GAME

;;Checks if either the snake hit itself OR the wall
(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

;;Check if the snakes head is on a body position
(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

;;Check if the snakes head is on a wall position
(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (render-end w)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

;;AUXILIARY FUNCTIONS
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-head sn)
  (first (snake-segs sn)))

(define (snake-body sn)
  (rest (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

;;BIG-BANG
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))



