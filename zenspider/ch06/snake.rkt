#lang racket

(require 2htdp/universe 2htdp/image)
(require racket/runtime-path)

;; TODO: use 'u 'd 'l 'r
;; TODO: rewrite opposite-dir? to use a hash or assoc
;; TODO: stop using string=? everywhere

(define SEG-SIZE 15)
(define SIZE 30)
(define HEIGHT-PX (* SEG-SIZE 30))
(define WIDTH-PX  (* SEG-SIZE 30))

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))

(define EXPIRATION-TIME 150)
(define TICK-RATE 1/10)

(define-runtime-path GOO-IMG-PATH  "goo.gif")
(define-runtime-path HEAD-IMG-PATH "head.gif")
(define-runtime-path SEG-IMG-PATH  "body.gif")

(define GOO-IMG  (bitmap/file GOO-IMG-PATH))
(define HEAD-IMG (bitmap/file HEAD-IMG-PATH))
(define SEG-IMG  (bitmap/file SEG-IMG-PATH))

(define HEAD-LEFT-IMG  HEAD-IMG)
(define HEAD-DOWN-IMG  (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG    (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

(struct pit   (snake goos) #:transparent)
(struct snake (dir segs)   #:transparent)
(struct posn  (x y)        #:transparent)
(struct goo   (loc expire) #:transparent)

(define snake-example
  (snake "up" (list (posn 1 1) (posn 1 2) (posn 1 3))))

(define goo-example
  (list (goo (posn 1 0) 3) (goo (posn 5 8) 15)))

(define pit-example
  (pit snake-example goo-example))

(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))

            (on-tick   next-pit TICK-RATE)
            (on-key    direct-snake)
            (to-draw   render-pit)
            (stop-when dead? render-end)))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))

  (if goo-to-eat
      (pit (grow snake)    (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

(define (can-eat snake goos)
  (cond [(empty? goos) false]
        [(close? (snake-head snake) (first goos)) (first goos)]
        [else (can-eat snake (rest goos))]))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (eat goos goos-to-eat)
  (cons (fresh-goo) (remove goos-to-eat goos)))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

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
    [("up")    (posn-move head  0 -1)]
    [("down")  (posn-move head  0  1)]
    [("left")  (posn-move head -1  0)]
    [("right") (posn-move head  1  0)])

  (cond [(string=? dir "up")    (posn-move head  0 -1)]
        [(string=? dir "down")  (posn-move head  0  1)]
        [(string=? dir "left")  (posn-move head -1  0)]
        [(string=? dir "right") (posn-move head  1  0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos)) (cons (fresh-goo) (renew (rest goos)))]
        [else (cons (first goos) (renew (rest goos)))]))

;; (define (renew goos)
;;   (cond [(empty? goos) empty]
;;         [else (cons (if (rotten? (first goos) (fresh-goo) (first goos)))
;;                     (renew (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

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
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-dir the-snake d) (pit-goos w))]))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

(define (original-opposite-dir? d1 d2)
  (cond [(string=? d1 "up")    (string=? d2 "down")]
        [(string=? d1 "down")  (string=? d2 "up")]
        [(string=? d1 "left")  (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

(define (better-opposite-dir? d1 d2)
  (case d1
    [("up")    (string=? d2 "down")]
    [("down")  (string=? d2 "up")]
    [("left")  (string=? d2 "right")]
    [("right") (string=? d2 "left")]))

(define OPPOSITES
  '(("up" "down") ("down" "up") ("left" "right") ("right" "left")))

(define (opposite-dir? d1 d2)
  (string=? (cadr (assoc d1 OPPOSITES)) d2))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define snake-body-scene (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))

  (img+scene (snake-head snake)
             (cond [(string=? dir "up")    HEAD-UP-IMG]
                   [(string=? dir "down")  HEAD-DOWN-IMG]
                   [(string=? dir "left")  HEAD-LEFT-IMG]
                   [(string=? dir "right") HEAD-RIGHT-IMG])
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

(define (original-goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))

  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

;; recursion is nice, but come on... you can't beat map here:
(define (goo-list+scene goos scene)
  (img-list+scene (map goo-loc goos) GOO-IMG scene))

(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (render-end w)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "black")
           (render-pit w))) ;; bug: (render-snake-world w)

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake)
  (define h (snake-head snake))
  (define x (posn-x h))
  (define y (posn-y h))

  (or (= x 0) (= x SIZE)
      (= y 0) (= y SIZE)))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-head sn)
  (first (snake-segs sn)))

(define (snake-body sn)
  (rest (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

;; (define (snake-part fun)
;;   (lambda (sn) (fun (snake-segs sn))))
;;
;; (define snake-head (snake-part first))
;; (define snake-body (snake-part rest))
;; (define snake-tail (snake-part last))

(start-snake)

;; (require data/queue)
;;
;; (struct game (tick pos trail) #:mutable #:transparent)
;; (struct pos (x y) #:transparent)
;; (struct hist pos (t) #:transparent)
;;
;; (define WIDTH  640)
;; (define HEIGHT 480)
;; (define BUBBLE-LIFE 50)
;; (define IMAGE-of-UFO (bitmap/file ufo-path))
;; (define PV 10)
;; (define NV (- PV))
;;
;; (define (new-game)
;;   (game 0
;;         (pos (/ WIDTH 2) (/ HEIGHT 2))
;;         (make-queue)))
;;
;; ;; TODO: define a macro to wrap up the let forms below
;;
;; (define (tick w)
;;   (let ((current (add1 (game-tick w)))
;;         (trail (game-trail w)))
;;
;;     (set-game-tick! w current)
;;
;;     (queue-filter! trail (lambda (h) (< (- current (hist-t h)) BUBBLE-LIFE))))
;;
;;   w)
;;
;; (define (draw-game-tick w scene)
;;   (let ((p (game-pos w)))
;;    (place-image/align
;;     (text (format "~s x ~s : ~s" (pos-x p) (pos-y p) (game-tick w)) 12 "black")
;;     10 10 'left 'top
;;     scene)))
;;
;; (define (draw-ufo w)
;;   (let ((p (game-pos w)))
;;     (place-image IMAGE-of-UFO (pos-x p) (pos-y p)
;;                  (draw-game-tick w
;;                                  (draw-trail w)))))
;;
;; (define (draw-trail w)
;;   (let* ((current (game-tick w))
;;          (trail   (game-trail w))
;;          (size    (lambda (h) (- current (hist-t h)))))
;;     (foldl (lambda (h r) (place-image (circle (size h) "outline" "black")
;;                                       (pos-x h)
;;                                       (pos-y h)
;;                                       r))
;;            (empty-scene WIDTH HEIGHT)
;;            (queue->list trail))))
;;
;; (define (move-ufo w x y)
;;   (let* ((p (game-pos w))
;;          (trail (game-trail w))
;;          (new-pos (pos (modulo (+ x (pos-x p)) WIDTH)
;;                        (modulo (+ y (pos-y p)) HEIGHT)))
;;          (new-hist (hist (pos-x new-pos) (pos-y new-pos) (game-tick w))))
;;
;;     (set-game-pos! w new-pos)
;;
;;     (enqueue! trail new-hist)
;;
;;     w))
;;
;; (define (key-handler w key)
;;   (cond [(key=? key "q")      (stop-with w)]
;;         [(key=? key "escape") (stop-with w)]
;;
;;         [(key=? key "up")     (move-ufo w 0 NV)]
;;         [(key=? key "down")   (move-ufo w 0 PV)]
;;         [(key=? key "left")   (move-ufo w NV 0)]
;;         [(key=? key "right")  (move-ufo w PV 0)]
;;
;;         [else w]))
;;
;; (big-bang (new-game)
;;           [on-tick tick]
;;           [to-draw draw-ufo]
;;           [on-key  key-handler])
