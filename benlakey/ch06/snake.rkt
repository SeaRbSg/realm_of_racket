#lang racket
(require 2htdp/universe 2htdp/image)

;; constants
(define TICK-RATE 1/5)
(define SIZE 30)

(define SEG-SIZE 30)

(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

;; primary data structures
(struct pit (snake goos) #:transparent)
(struct snake (direction segments) #:transparent)
(struct point (x y) #:transparent)
(struct goo (location expiration-ticks) #:transparent)

;; control functions
(define (direct-snake w key-event)
  (cond [(dir? key-event) (world-change-dir w key-event)]
        [else w]))

(define (dir? x)
      (or (key=? x UP)
          (key=? x DOWN)
          (key=? x LEFT)
          (key=? x RIGHT)))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 UP) (string=? d2 DOWN)]
        [(string=? d1 DOWN) (string=? d2 UP)]
        [(string=? d1 LEFT) (string=? d2 RIGHT)]
        [(string=? d1 RIGHT) (string=? d2 LEFT)]))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-direction the-snake) d)
              (cons? (rest (snake-segments the-snake))))
         (stop-with w)]
         [else
          (pit (snake-change-dir the-snake d) (pit-goos w))]))

;; snake funcs
(define (snake-head sn)
  (first (snake-segments sn)))

(define (snake-body sn)
  (rest (snake-segments sn)))

(define (snake-tail sn)
  (last (snake-segments sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segments sn)))

;; point funcs
(define (point=? p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(define (point-move p dx dy)
  (point (+ (point-x p) dx)
         (+ (point-y p) dy)))

;; goo funcs
(define (fresh-goo)
  (goo (point (+ 1 (random (sub1 SIZE)))
              (+ 1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

(define (rotten? g)
  (zero? (goo-expiration-ticks g)))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos)) 
          (cons (fresh-goo) (renew (rest goos)))]
        [else 
          (cons (first goos) (renew (rest goos)))]))

(define (decay g)
  (goo (goo-location g) (sub1 (goo-expiration-ticks g))))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (age-goo goos)
  (rot (renew goos)))

;; misc funcs
(define (next-head sn)
  (define head (snake-head sn))
  (define direction (snake-direction sn))
  (cond [(string=? direction UP) (point-move head 0 -1)]
        [(string=? direction DOWN) (point-move head 0 1)]
        [(string=? direction LEFT) (point-move head -1 0)]
        [(string=? direction RIGHT) (point-move head 1 0)]))

(define (all-but-last segments)
  (cond [(empty? (rest segments)) empty]
        [else (cons (first segments) (all-but-last (rest segments)))]))

(define (slither sn)
  (snake (snake-direction sn)
         (cons (next-head sn) (all-but-last (snake-segments sn)))))

(define (grow sn)
  (snake (snake-direction sn)
         (cons (next-head sn) (snake-segments sn))))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (close? snake goo)
  (point=? snake (goo-location goo)))

(define (can-eat sn goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head sn) (first goos))
                  (first goos)
                  (can-eat sn (rest goos)))]))

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake)
  (define x (point-x (snake-head snake))) 
  (define y (point-y (snake-head snake))) 
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (number-of-goos w)
  (define goos (pit-goos w))
  (length goos))

;; game stepping function
(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))


;; rendering
(define (render-goos-remaining w)
  (text (string-append 
         "Goos remaining:" 
         (number->string (number-of-goos w)))
        ENDGAME-TEXT-SIZE 
        "black"))

(define (render-game-over)
  (text "Game Over" ENDGAME-TEXT-SIZE "black"))

(define (render-end w)
  (overlay (overlay/offset (render-game-over)
                           0 ENDGAME-TEXT-SIZE
                           (render-goos-remaining w))
           (render-pit w)))

(define (goo-list+scene goos scene)
  (define (get-points-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-location (first goos))
                      (get-points-from-goo (rest goos)))]))
  (img-list+scene (get-points-from-goo goos) GOO-IMG scene))

(define (img+scene point img scene)
  (place-image img
               (* (point-x point) SEG-SIZE)
               (* (point-y point) SEG-SIZE)
               scene))

(define (img-list+scene points img scene)
  (cond [(empty? points) scene]
        [else (img+scene
               (first points)
               img
               (img-list+scene (rest points) img scene))]))

(define (snake+scene snake scene)
      (define snake-body-scene
        (img-list+scene  (snake-body snake) SEG-IMG scene))
      (define dir (snake-direction snake))
      (img+scene (snake-head snake)
                 (cond [(string=? UP dir) HEAD-UP-IMG]
                       [(string=? DOWN dir) HEAD-DOWN-IMG]
                       [(string=? LEFT dir) HEAD-LEFT-IMG]
                       [(string=? RIGHT dir) HEAD-RIGHT-IMG])
                 snake-body-scene))

(define (render-pit w)
      (snake+scene (pit-snake w)
                   (goo-list+scene (pit-goos w) MT-SCENE)))


;; main function
(define (start-snake)
  (big-bang (pit (snake RIGHT (list (point 1 1)))
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

