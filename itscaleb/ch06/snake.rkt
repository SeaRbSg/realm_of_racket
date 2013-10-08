#lang racket
(require 2htdp/image 2htdp/universe)

(struct pit (snake goos) #:transparent)
(struct snake (direction segments) #:transparent)
(struct goo (location expire) #:transparent)
(struct position (x y) #:transparent)
(define TICK-RATE 1/10)
(define SIZE 30)
(define EXPIRATION-TIME 150)
(define ENDGAME-TEXT-SIZE 15)
(define SEG-SIZE 15)
(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "goo.gif"))
(define SEG-IMG  (bitmap "body.gif"))
(define HEAD-IMG (bitmap "head.gif"))
(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define (start-snake)
  (big-bang (pit (snake "right" (list (position 1 1)))
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

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (equal? (snake-head snake) (goo-location(first goos)))
              (first goos)
              (can-eat snake (rest goos)))]))

(define (snake-head snake)
  (first (snake-segments snake)))

(define (fresh-goo)
  (goo (position (add1 (random (sub1 SIZE)))
                 (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene  (snake-body snake) SEG-IMG scene))
  (define dir (snake-direction snake))
  (img+scene (snake-head snake) 
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

(define (img+scene posn img scene)
  (place-image img 
               (* (position-x posn) SEG-SIZE)
               (* (position-y posn) SEG-SIZE)
               scene))

(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns)
                         img 
                         (img-list+scene (rest posns) img scene))]))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-location (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

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
  (cond [(and (opposite-dir? (snake-direction the-snake) d)
              (cons? (rest (snake-segments the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-dir the-snake d) (pit-goos w))]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

(define (snake-change-dir sn d)
  (snake d (snake-segments sn)))


(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (render-end w)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (snake-body sn)
  (rest (snake-segments sn)))

(define (wall-colliding? snake)
  (define x (position-x (snake-head snake)))
  (define y (position-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (grow sn)
  (snake (snake-direction sn)
         (cons (next-head sn) (snake-segments sn))))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-direction sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (posn-move pos dx dy)
  (position (+ (position-x pos) dx)
            (+ (position-y pos) dy)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))

(define (rotten? goo)
  (zero? (goo-expire goo)))

(define (decay g)
  (goo (goo-location g) (sub1 (goo-expire g))))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (slither sn)
  (snake (snake-direction sn)
         (cons (next-head sn) (all-but-last (snake-segments sn)))))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (position=? p1 p2)
  (and (= (position-x p1) (position-x p2))
       (= (position-y p1) (position-y p2))))

(module+ test
  (require rackunit)
  (check-true (position=? (position 1 1) (position 1 1)))
  
  (define test-snake (snake "up" (list (position 1 1))))
  (define test-goos (list (goo (position 1 1) 4)))
  (check-equal? (can-eat test-snake test-goos) (goo (position 1 1) 4))
  
  (set! test-snake (snake "up" (list (position 3 3))))
  (set! test-goos (list (goo (position 2 3) 4) (goo (position 3 3) 4)))
  (check-equal? (can-eat test-snake test-goos) (goo (position 3 3) 4))
  
  (set! test-snake (snake "up" (list (position 1 1))))
  (set! test-goos (list (goo (position 1 2) 4)))
  (check-false (can-eat test-snake test-goos))
  
  (set! test-snake (snake "up" (list (position 2 3) (position 3 3))))
  (check-equal? (snake-head test-snake) (position 2 3))
  
  (set! test-snake (snake "up" (list (position 2 3) (position 3 3))))
  (check-equal? (next-head test-snake) (position 2 2))
  
  (set! test-snake (snake "down" (list (position 2 3) (position 3 3))))
  (check-equal? (next-head test-snake) (position 2 4))
  
  (set! test-snake (snake "left" (list (position 2 3) (position 3 3))))
  (check-equal? (next-head test-snake) (position 1 3))
  
  (set! test-snake (snake "right" (list (position 2 3) (position 3 3))))
  (check-equal? (next-head test-snake) (position 3 3))
  )