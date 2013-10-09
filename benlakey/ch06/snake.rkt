#lang racket
(require 2htdp/universe 2htdp/image)

;; primary data structures
(struct pit (snake goos) #:transparent)
(struct snake (direction segments) #:transparent)
(struct point (x y) #:transparent)
(struct goo (location expiration-ticks) #:transparent)

;; game stepping functions
(define (next-pit w)

  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

;; main function
(define (start-snake)
  (big-bang (pit (snake "right" (list (point 1 1)))
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

