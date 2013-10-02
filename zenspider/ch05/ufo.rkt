#lang racket

(require 2htdp/universe 2htdp/image)
(require data/queue)
(require racket/runtime-path)

(struct game (tick pos trail) #:mutable #:transparent)
(struct pos (x y) #:transparent)
(struct hist pos (t) #:transparent)

(define-runtime-path ufo-path "ufo.png")
(define WIDTH  640)
(define HEIGHT 480)
(define BUBBLE-LIFE 50)
(define IMAGE-of-UFO (bitmap/file ufo-path))
(define PV 10)
(define NV (- PV))

(define (new-game)
  (game 0
        (pos (/ WIDTH 2) (/ HEIGHT 2))
        (make-queue)))

;; TODO: define a macro to wrap up the let forms below

(define (tick w)
  (let ((current (add1 (game-tick w)))
        (trail (game-trail w)))

    (set-game-tick! w current)

    (queue-filter! trail (lambda (h) (< (- current (hist-t h)) BUBBLE-LIFE))))

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
  (let* ((current (game-tick w))
         (trail   (game-trail w))
         (size    (lambda (h) (- current (hist-t h)))))
    (foldl (lambda (h r) (place-image (circle (size h) "outline" "black")
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
