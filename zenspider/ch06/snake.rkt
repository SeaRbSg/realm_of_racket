#lang racket

(require 2htdp/universe 2htdp/image)
(require racket/runtime-path)

;;; consts

;; time stuff

(define EXPIRATION-TIME 150)
(define TICK-RATE 1/10)

;; size stuff

(define SEG-SIZE 15)
(define ENDGAME-TEXT-SIZE 72)
(define SIZE 30)
(define HEIGHT-PX (* SEG-SIZE 30))
(define WIDTH-PX  (* SEG-SIZE 30))
(define TRIGGER-TIME (+ 5 (current-seconds)))

;; image stuff

(define MT-SCENE #f)

(define-runtime-path GOO-IMG-PATH  "goo.gif")
(define-runtime-path GOO2-IMG-PATH  "goo2.png")
(define-runtime-path HEAD-IMG-PATH "head.gif")
(define-runtime-path SEG-IMG-PATH  "body.gif")

(define GOO-IMG  (bitmap/file GOO-IMG-PATH))
(define GOO2-IMG  (bitmap/file GOO2-IMG-PATH))
(define HEAD-IMG (bitmap/file HEAD-IMG-PATH))
(define SEG-IMG  (bitmap/file SEG-IMG-PATH))

(define HEAD-LEFT-IMG  HEAD-IMG)
(define HEAD-DOWN-IMG  (rotate 90       HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG    (flip-vertical   HEAD-DOWN-IMG))

;; direction/mapping stuff

(define OPPOSITES
  '((u d) (l r)
    (d u) (r l)))

(define DIRECTIONS
  '(("up" u) ("down" d) ("left" l) ("right" r)))

(define DIR-TO-HEAD-IMG
  (list (list 'u HEAD-UP-IMG)
        (list 'd HEAD-DOWN-IMG)
        (list 'l HEAD-LEFT-IMG)
        (list 'r HEAD-RIGHT-IMG)))

;;; structs

(struct pit   (snake goos walls) #:transparent)
(struct snake (dir segs)   #:transparent)
(struct posn  (x y)        #:transparent)
(struct goo   (loc expire score) #:transparent)

;;; top level game stuff

(define (start-snake)
  (big-bang (pit (snake 'r (list (posn 1 1)))
                 (build-list (add1 (random 10)) (lambda (x) (fresh-goo)))
                 (randomize-walls))

            (on-tick   next-pit TICK-RATE)
            (on-key    direct-snake)
            (to-draw   render-pit)
            (stop-when dead? render-end)))

(define (randomize-walls)
  (let ((walls (build-list (random 5) (lambda (x) (random-pos)))))
    (set! MT-SCENE
          (foldl (lambda (p sc) (img+scene p (square SIZE 'solid 'black) sc))
                 (empty-scene WIDTH-PX HEIGHT-PX)
                 walls))
    walls))

(define (copy-pit p new-snake new-goos new-walls)
  (pit (or new-snake (pit-snake p))
       (or new-goos  (pit-goos  p))
       (or new-walls (pit-walls p))))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (define walls (pit-walls w))

  (when (>= (current-seconds) TRIGGER-TIME)
    (set! TRIGGER-TIME (+ 5 (current-seconds)))
    (set! walls (randomize-walls)))

  (if goo-to-eat
      (copy-pit w
                (grow snake (goo-score goo-to-eat))
                (age-goo (eat goos goo-to-eat))
                walls)
      (copy-pit w (slither snake) (age-goo goos) walls)))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))

  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else
         (copy-pit w (snake-change-dir the-snake d) #f #f)]))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define snake-body-scene (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))

  (img+scene (snake-head snake)
             (cadr (assoc dir DIR-TO-HEAD-IMG))
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

(define GOO-IMGS
  (list #f GOO-IMG GOO2-IMG))

;; recursion is nice, but come on... you can't beat map here:
(define (goo-list+scene goos scene)
  (foldl (lambda (g s)
           (let ((img (list-ref GOO-IMGS (goo-score g))))
             (img+scene (goo-loc g) img s))) scene goos))

(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake (pit-walls w))))

(define (render-end w)
  (let* ((mid (/ WIDTH-PX 2))
         (game-over  (text "Game Over" ENDGAME-TEXT-SIZE "black"))
         (score-text (format "Score = ~s" (snake-score (pit-snake w))))
         (score      (text score-text ENDGAME-TEXT-SIZE "black")))
    (place-image/align game-over mid 0 "middle" "top"
                       (place-image/align score mid HEIGHT-PX "middle" "bottom"
                                          (render-pit w)))))

;;; snake stuff

(define (can-eat snake goos)
  (cond [(empty? goos) false]
        [(close? (snake-head snake) (first goos)) (first goos)]
        [else (can-eat snake (rest goos))]))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (grow sn n)
  (define grow-1 (lambda (sn)
                   (snake (snake-dir sn) (cons (next-head sn) (snake-segs sn)))))
  (if (> n 1)
      (grow (slither (grow-1 sn)) (sub1 n))
      (grow-1 sn)))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir  (snake-dir sn))

  (case dir
    [(u) (posn-move head  0 -1)]
    [(d) (posn-move head  0  1)]
    [(l) (posn-move head -1  0)]
    [(r) (posn-move head  1  0)]))

(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w (map-direction ke))]
        [else w]))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake walls)
  (define h (snake-head snake))
  (define x (posn-x h))
  (define y (posn-y h))

  (or (= x 0) (= x SIZE)
      (= y 0) (= y SIZE)
      (cons? (member (snake-head snake) walls))))

(define (snake-part fun)
  (lambda (sn) (fun (snake-segs sn))))

(define snake-head (snake-part first))
(define snake-body (snake-part rest))
(define snake-tail (snake-part last))

(define (snake-score sn)
  (length (snake-segs sn)))

;;; goo stuff

(define (eat goos goos-to-eat)
  (cons (fresh-goo) (remove goos-to-eat goos)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-score g)))

(define (renew goos)
  (cond [(empty? goos) empty]
        [else (cons (if (rotten? (first goos)) (fresh-goo) (first goos))
                    (renew (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (random-pos)
  (posn (add1 (random (sub1 SIZE)))
        (add1 (random (sub1 SIZE)))))

(define (fresh-goo)
  (goo (random-pos)
       EXPIRATION-TIME
       (add1 (random 2))))

;;; misc support

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (map-direction d)
  (cadr (assoc d DIRECTIONS)))

(define (dir? x)
  (cons? (assoc x DIRECTIONS)))

(define (opposite-dir? d1 d2)
  (eq? (cadr (assoc d1 OPPOSITES)) d2))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;;; DONE

(start-snake)
