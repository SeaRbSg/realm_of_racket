#lang racket

(require 2htdp/image 2htdp/universe "shared.rkt")

(provide launch-guess-client)

(define TEXT-SIZE 11)
(define HELP-TEXT
  (text "↑ for larger numbers, ↓ for smaller ones"
        TEXT-SIZE
        "blue"))
(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define COLOR "red")
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y
   "left" "top"
   (place-image/align
    HELP-TEXT2
    TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

;; clientstate is a string
(define clientstate0 "none available")

(define (launch-guess-client n)
  (big-bang clientstate0
            (to-draw draw-guess)
            (on-key handle-keys)
            (name n)
            (register n)
            (on-receive handle-msg)))

(define (handle-msg c msg)
  (number->string msg))

(define (handle-keys w key)
  (case (string->symbol key)
    [(up)   (make-package w "up")]
    [(down) (make-package w "down")]
    [(q)    (stop-with w)]
    ((=)    (stop-with w))
    [else   w]))

(define (draw-guess c)
  (overlay (text c SIZE COLOR) MT-SC))
