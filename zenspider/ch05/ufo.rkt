#lang racket

(require 2htdp/universe 2htdp/image)
(require data/queue)

(struct game (tick pos trail) #:mutable #:transparent)
(struct pos (x y) #:transparent)

(define WIDTH  640)
(define HEIGHT 480)
(define IMAGE-of-UFO (bitmap/file "ch05/ufo.png"))
(define PV 10)
(define NV (- PV))

(define (new-game)
  (game 0
        (pos (/ WIDTH 2) (/ HEIGHT 2))
        '()))

(define (tick w)
  (set-game-tick! w (add1 (game-tick w)))
  w)

(define (draw-game-tick w)
  (let ((p (game-pos w)))
   (place-image/align
    (text (format "~s x ~s : ~s" (pos-x p) (pos-y p) (game-tick w)) 12 "black")
    10 10 'left 'top
    (empty-scene WIDTH HEIGHT))))

(define (draw-ufo w)
  (let ((p (game-pos w)))
    (place-image IMAGE-of-UFO (pos-x p) (pos-y p)
                 (draw-game-tick w))))

(define (move-ufo w x y)
  (let ((p (game-pos w)))
    (set-game-pos! w (pos (modulo (+ x (pos-x p)) WIDTH)
                          (modulo (+ y (pos-y p)) HEIGHT)))
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
