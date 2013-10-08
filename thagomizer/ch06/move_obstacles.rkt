#lang racket
(require 2htdp/image 2htdp/universe)

;;
;; STRUCTURES
;;

(struct goo (loc expire type) #:transparent)
(struct wall (loc))
(struct posn (x y) #:transparent)
(struct snake (dir segs) #:transparent)
(struct pit (snake goos walls goos-eaten wall-expire) #:transparent)

;;
;; CONSTANTS
;;

;; General
(define TICK-RATE 1/5)
(define SIZE 25)
(define SEG-SIZE 25)
(define WIDTH-PX  (* SEG-SIZE SIZE))
(define HEIGHT-PX (* SEG-SIZE SIZE))
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define ENDGAME-TEXT-SIZE 15)

;; Snake
(define SEG-IMG  (bitmap "body.gif"))
(define HEAD-IMG (bitmap "head.gif"))
(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

;; Goo
(define GOO-LIFE 150)
(define MAX-GOOS 10)
(define REGULAR-GOO 1)
(define SUPER-GOO 2)
(define GOO-SIZE 10)
(define SUPER-PROB 0.5)
(define GOO-IMG (bitmap "goo.gif"))
(define SUPER-GOO-IMG (bitmap "blue_goo.gif"))

;; Wall
(define WALL-SIZE 20)
(define WALL-IMG (square WALL-SIZE "solid" "black"))
(define WALL-LIFE 50)
(define WALL-COUNT 10)

;; 
;; Functions
;;

;; Main Runner
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (fresh-goos (add1 (random (sub1 MAX-GOOS))))
                 (fresh-walls WALL-COUNT)
                 0
                 WALL-LIFE)
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

;; General Utility 
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns)
                         img 
                         (img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

;; Game specific utility
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (define goos-eaten (pit-goos-eaten w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              ;; consists of the head and at least one segment
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else 
         (pit (snake-change-dir the-snake d)
              (pit-goos w)
              (pit-walls w)
              goos-eaten
              (pit-wall-expire w))]))

(define (direct-snake w key)
  (cond [(dir? key) (world-change-dir w key)]
        [else w]))

(define (render-end w)
  (overlay (text 
            (string-append "Game Over. " 
                           (number->string (pit-goos-eaten w))
                           " goos eaten!")
            ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

;; Pit
(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define walls
    (if (zero? (pit-wall-expire w))
        (fresh-walls WALL-COUNT)
        (pit-walls w)))
  (define wall-expire
    (if (zero? (pit-wall-expire w))
        WALL-LIFE
        (sub1 (pit-wall-expire w))))
  (define goos-eaten (pit-goos-eaten w))
  (define goo-to-eat (can-eat snake goos))
    (if goo-to-eat
      (pit 
       (grow snake (goo-type goo-to-eat)) 
       (age-goo (eat goos goo-to-eat)) 
       walls
       (add1 goos-eaten)
       wall-expire)
      (pit 
       (slither snake) 
       (age-goo goos) 
       walls
       goos-eaten
       wall-expire)))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (walls+scene (pit-walls w)
                                (goo-list+scene (pit-goos w) MT-SCENE))))

;; Goo
(define (super? g)
  (equal? (goo-type g) SUPER-GOO))

(define (fresh-goos n)
  (cond [(zero? n) empty]
        [(cons (fresh-goo) (fresh-goos (sub1 n)))]))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       GOO-LIFE
       (new-goo-type)))

(define (new-goo-type)
  (if (< (random) SUPER-PROB)
      SUPER-GOO
      REGULAR-GOO))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-type g)))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else (cons (first goos) (renew (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (img-for-goo goo)
  (cond [(super? goo) SUPER-GOO-IMG]
        [else GOO-IMG]))

(define (goo-list+scene goos scene)
  (cond [(empty? goos) scene]
        [else 
         (define goo (first goos))
         (define posn (goo-loc goo))
         (define img (img-for-goo goo))
         (img+scene posn 
                    img 
                    (goo-list+scene (rest goos) scene))]))


;; Snake
(define (snake-head sn)
  (first (snake-segs sn)))

(define (snake-body sn)
  (rest (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))


(define (grow sn grow-by)
  (define new-snake (snake 
                     (snake-dir sn) 
                     (cons (next-head sn) (snake-segs sn))))
  (cond [(equal? grow-by 1) new-snake]
        [else (grow new-snake (sub1 grow-by))]))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (dead? w)
  (define snake (pit-snake w))
  (define head (snake-head snake))
  (define walls (pit-walls w))
  (or (self-colliding? snake) (edge-colliding? snake) (wall-colliding? walls head)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake)
                 (snake-body snake))))

(define (edge-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (wall-colliding? walls head-posn)
  (cond [(empty? walls) #f]
        [(posn=? head-posn (wall-loc (first walls))) #t]
        [else (wall-colliding? (rest walls) head-posn)]))

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake)
                    SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))



;; Walls
(define (fresh-walls n)
  (cond [(zero? n) empty]
        [else 
         (cons (fresh-wall) (fresh-walls (sub1 n)))]))

(define (fresh-wall)
  (wall (posn (add1 (random (sub1 SIZE)))
                  (add1 (random (sub1 SIZE))))))
            
(define (walls+scene walls scene)
  (cond [(empty? walls) scene]
        [else
         (define wall (first walls))
         (define posn (wall-loc wall))
         (img+scene posn WALL-IMG (walls+scene (rest walls) scene))]))







