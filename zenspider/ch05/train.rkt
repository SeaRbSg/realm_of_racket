#lang racket

(require 2htdp/universe 2htdp/image)

(define WIDTH  640)
(define HEIGHT 480)

(define (move-train current-state)
  (+ current-state 6))

(define (draw-a-train-onto-an-empty-scene current-state)
  (place-image
   IMAGE-of-TRAIN current-state (/ HEIGHT 2)
   (empty-scene WIDTH HEIGHT)))

(define IMAGE-of-TRAIN (bitmap/file "ch05/ufo.png"))

(define (state-is-300 current-state)
  (>= current-state 300))

(define (key-handler w key)
  (cond [(key=? key "q")      (stop-with w)]
        [(key=? key "escape") (stop-with w)]
        [else w]))

(big-bang 0
          [on-tick move-train]
          [to-draw draw-a-train-onto-an-empty-scene]
          [on-key  key-handler])
