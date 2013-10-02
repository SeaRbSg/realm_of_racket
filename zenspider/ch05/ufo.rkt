#lang racket

(require 2htdp/universe 2htdp/image)
(require data/queue)

(struct game (tick pos trail) #:mutable #:transparent)
(struct pos (x y) #:transparent)
(struct hist pos (t) #:transparent)

(define WIDTH  640)
(define HEIGHT 480)
(define MAX-BUBBLES 50)
(define IMAGE-of-UFO (bitmap/file "ch05/ufo.png"))
(define PV 10)
(define NV (- PV))

(define (new-game)
  (game 0
        (pos (/ WIDTH 2) (/ HEIGHT 2))
        (make-queue)))

(define (pos-=? a b)
  (and a b
       (equal? (pos-x a) (pos-x b))
       (equal? (pos-y a) (pos-y b))))

(define (tick w)
  (set-game-tick! w (add1 (game-tick w)))

  w)

(define (draw-game-tick w scene)
  (let ((p (game-pos w)))
   (place-image/align
    (text (format "~s x ~s : ~s" (pos-x p) (pos-y p) (game-tick w)) 12 "black")
    10 10 'left 'top
    scene)))

(define (draw-ufo w)
  (let ((p (game-pos w)))
    (place-image IMAGE-of-UFO (pos-x p) (pos-y p)
                 (draw-game-tick w
                                 (draw-trail w)))))

(define (draw-trail w)
  (let ((trail (game-trail w))
        (size 5))
    (foldl (lambda (h r) (place-image (circle size "outline" "black")
                                      (pos-x h)
                                      (pos-y h)
                                      r))
           (empty-scene WIDTH HEIGHT)
           (queue->list trail))))

(define (move-ufo w x y)
  (let* ((p (game-pos w))
         (trail (game-trail w))
         (new-pos (pos (modulo (+ x (pos-x p)) WIDTH)
                       (modulo (+ y (pos-y p)) HEIGHT)))
         (new-hist (hist (pos-x new-pos) (pos-y new-pos) (game-tick w))))

    (set-game-pos! w new-pos)

    (enqueue! trail new-hist)

    (when (> (queue-length trail) MAX-BUBBLES)
          (dequeue! trail))

    w))

(define (key-handler w key)
  (cond [(key=? key "q")      (stop-with w)]
        [(key=? key "escape") (stop-with w)]

        [(key=? key "up")     (move-ufo w 0 NV)]
        [(key=? key "down")   (move-ufo w 0 PV)]
        [(key=? key "left")   (move-ufo w NV 0)]
        [(key=? key "right")  (move-ufo w PV 0)]

        [else w]))

(big-bang (new-game)
          [on-tick tick]
          [to-draw draw-ufo]
          [on-key  key-handler])
