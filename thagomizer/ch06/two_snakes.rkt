#lang racket
(require 2htdp/image 2htdp/universe)

;;
;; STRUCTURES
;;

(struct goo (loc expire type) #:transparent)
(struct posn (x y) #:transparent)
(struct snake (dir segs) #:transparent)
(struct pit (snake1 snake2 goos goos-eaten1 goos-eaten2) #:transparent)

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
(define HEAD2-IMG (bitmap "purple_head.gif"))
(define HEAD2-LEFT-IMG HEAD2-IMG)
(define HEAD2-DOWN-IMG (rotate 90 HEAD2-LEFT-IMG))
(define HEAD2-RIGHT-IMG (flip-horizontal HEAD2-LEFT-IMG))
(define HEAD2-UP-IMG (flip-vertical HEAD2-DOWN-IMG))

;; Goo
(define EXPIRATION-TIME 150)
(define MAX-GOOS 10)
(define REGULAR-GOO 1)
(define SUPER-GOO 2)
(define GOO-SIZE 10)
(define SUPER-PROB 0.5)
(define GOO-IMG (bitmap "goo.gif"))
(define SUPER-GOO-IMG (circle GOO-SIZE "solid" "magenta"))

;; 
;; Functions
;;

;; Main Runner
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (snake "left" (list (posn 10 10)))
                 (fresh-goos (add1 (random (sub1 MAX-GOOS))))
                 0
                 0)
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
      (key=? x "right")
      (key=? x "a")
      (key=? x "o")
      (key=? x "e")
      (key=? x ",")))

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

(define (world-change-dir w snake-num d)
  (define goos-eaten1 (pit-goos-eaten1 w))
  (define goos-eaten2 (pit-goos-eaten2 w))
  (cond [(equal? snake-num 1)
         (define snake (pit-snake1 w))
         (cond [(and (opposite-dir? (snake-dir snake) d)
                     ;; consists of the head and at least one segment
                     (cons? (rest (snake-segs snake))))
                (stop-with w)]
               [else 
                (pit (snake-change-dir snake d)
                     (pit-snake2 w)
                     (pit-goos w)
                     goos-eaten1
                     goos-eaten2)])]
        [else
         (define snake (pit-snake2 w))
         (cond [(and (opposite-dir? (snake-dir snake) d)
                     ;; consists of the head and at least one segment
                     (cons? (rest (snake-segs snake))))
                (stop-with w)]
               [else 
                (pit (pit-snake1 w)
                     (snake-change-dir snake d)
                     (pit-goos w)
                     goos-eaten1
                     goos-eaten2)])]))        

(define (direct-snake w key)
  (define snake-num (determine-snake-num key))
  (cond [(dir? key) (world-change-dir w snake-num (direction-from-key key))]
        [else w]))

(define (direction-from-key key)
  (cond [(key=? key "up") "up"]
        [(key=? key "down") "down"]
        [(key=? key "left") "left"]
        [(key=? key "right") "right"]
        [(key=? key "a") "left"]
        [(key=? key "e") "right"]
        [(key=? key "o") "down"]
        [(key=? key ",") "up"]))

(define (determine-snake-num key)
  (if (or (key=? key "up")
          (key=? key "down")
          (key=? key "left")
          (key=? key "right"))
      1
      2))

(define (render-end w)
  (overlay (text 
            (string-append "Game Over. Snake 1 ate " 
                           (number->string (pit-goos-eaten1 w))
                           " goos! Snake 2 ate "
                           (number->string (pit-goos-eaten2 w))
                           " goos!")
            ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

;; Pit
(define (next-pit w)
  (define snake1 (pit-snake1 w))
  (define snake2 (pit-snake2 w))
  (define goos (pit-goos w))
  (define goos-eaten1 (pit-goos-eaten1 w))
  (define goos-eaten2 (pit-goos-eaten2 w))
  (define goo-to-eat1 (can-eat snake1 goos))
  (define goo-to-eat2 (can-eat snake2 goos))
  (define new-snake1 
    (if goo-to-eat1
        (begin
          (set! goos (eat goos goo-to-eat1))
          (set goos-eaten1 (add1 goos-eaten1))
          (grow snake1 (goo-type goo-to-eat1)))
        (slither snake1)))
    (define new-snake2
    (if goo-to-eat2
        (begin
          (set! goos (eat goos goo-to-eat2))
          (set goos-eaten2 (add1 goos-eaten2))
          (grow snake2 (goo-type goo-to-eat2)))
        (slither snake2)))
  (pit 
   new-snake1
   new-snake2
   (age-goo goos)
   goos-eaten1
   goos-eaten2))

(define (render-pit w)
  (snake+scene (pit-snake1 w)
               HEAD-IMG
               (snake+scene (pit-snake2 w)
                            HEAD2-IMG
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
       EXPIRATION-TIME
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
  (define snake1 (pit-snake1 w))
  (define snake2 (pit-snake2 w))
  (or (self-colliding? snake1) 
      (wall-colliding? snake1)
      (self-colliding? snake2)
      (wall-colliding? snake2)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake)
                 (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (snake+scene snake head-img scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake)
                    SEG-IMG scene))
  (define dir (snake-dir snake))
  (if (equal? head-img HEAD-IMG)
      (img+scene (snake-head snake)
                 (cond [(string=? "up" dir) HEAD-UP-IMG]
                       [(string=? "down" dir) HEAD-DOWN-IMG]
                       [(string=? "left" dir) HEAD-LEFT-IMG]
                       [(string=? "right" dir) HEAD-RIGHT-IMG])
                 snake-body-scene)
      (img+scene (snake-head snake)
                 (cond [(string=? "up" dir) HEAD2-UP-IMG]
                       [(string=? "down" dir) HEAD2-DOWN-IMG]
                       [(string=? "left" dir) HEAD2-LEFT-IMG]
                       [(string=? "right" dir) HEAD2-RIGHT-IMG])
                 snake-body-scene)))





