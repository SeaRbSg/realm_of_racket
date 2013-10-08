#lang racket
(require 2htdp/image 2htdp/universe)

(struct goo (loc expire type) #:transparent)
(struct posn (x y) #:transparent)
(struct snake (dir segs) #:transparent)
(struct pit (snake goos goos-eaten) #:transparent)

;; CONSTANTS
(define TICK-RATE 1/5)
(define SIZE 25)
(define SEG-SIZE 25)
(define EXPIRATION-TIME 150)
(define MAX-GOOS 10)
(define REGULAR-GOO 1)
(define SUPER-GOO 2)
(define GOO-SIZE 10)
(define SUPER-PROB 0.5)

(define WIDTH-PX  (* SEG-SIZE SIZE))
(define HEIGHT-PX (* SEG-SIZE SIZE))

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "goo.gif"))
(define SUPER-GOO-IMG (circle GOO-SIZE "solid" "magenta"))
(define SEG-IMG  (bitmap "body.gif"))
(define HEAD-IMG (bitmap "head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (fresh-goos (add1 (random (sub1 MAX-GOOS))))
                 0)
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (fresh-goos n)
  (cond [(zero? n) empty]
        [(cons (fresh-goo) (fresh-goos (sub1 n)))]))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goos-eaten (pit-goos-eaten w))
  (define goo-to-eat (can-eat snake goos))
    (if goo-to-eat
      (pit (grow snake (goo-type goo-to-eat)) (age-goo (eat goos goo-to-eat)) (add1 goos-eaten))
      (pit (slither snake) (age-goo goos) goos-eaten)))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (grow sn grow-by)
  (define new-snake (snake 
                     (snake-dir sn) 
                     (cons (next-head sn) (snake-segs sn))))
  (cond [(equal? grow-by 1) new-snake]
        [else (grow new-snake (sub1 grow-by))]))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

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

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME
       (new-goo-type)))

(define (new-goo-type)
  (if (< (random) SUPER-PROB)
      SUPER-GOO
      REGULAR-GOO))

(define (direct-snake w key)
  (cond [(dir? key) (world-change-dir w key)]
        [else w]))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

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
              goos-eaten)]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

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

(define (super? g)
  (equal? (goo-type g) SUPER-GOO))

(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (render-end w)
  (overlay (text 
            (string-append "Game Over. " 
                           (number->string (pit-goos-eaten w))
                           " goos eaten!")
            ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake)
                 (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

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
