#lang racket
(require 2htdp/image)

;; 4.5.1 Module-Level Definitions
(define WIDTH 100)
(define HEIGHT 200)

(define X-CENTER (quotient WIDTH 2))
(define Y-CENTER (quotient HEIGHT 2))

(unless (> HEIGHT 0)
  (error 'guess-my-number "HEIGHT may not be negative"))

(define SQR-COLOR 'red)
(define SQR-SIZE 10)
(define (draw-square img x y)
  (place-image (square SQR-SIZE "solid" SQR-COLOR)
               x y
               img))

;; 4.5.2 Local Definitions
(struct posn (x y))
(struct rect (width height))
(define (inside-of-rectangle? r p)
  (define x (posn-x p))
  (define y (posn-y p))
  (define width (rect-width r))
  (define height (rect-height r))
  (and (<= 0 x) (< x width) (<= 0 y) (< y height)))