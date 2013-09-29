#lang racket
(require 2htdp/universe 2htdp/image)

(struct guessing-state (small big guess-count))

(define TEXT-SIZE 14)
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
(define COUNT-SIZE 36)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define TEXT-MIDDLE-Y 62)

(define (MT-SC w)
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    (text (number->string (guessing-state-guess-count w)) COUNT-SIZE COLOR) TEXT-X TEXT-MIDDLE-Y "left" "middle"
    (place-image/align
     HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
     (empty-scene WIDTH HEIGHT)))))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (smaller w)
  (guessing-state (guessing-state-small w)
            (max (guessing-state-small w) (sub1 (guess w)))
            (add1 (guessing-state-guess-count w))))

(define (bigger w)
  (guessing-state (min (guessing-state-big w) (add1 (guess w)))
            (guessing-state-big w)
            (add1 (guessing-state-guess-count w))))

(define (guess w)
  (quotient (+ (guessing-state-small w) (guessing-state-big w)) 2))


(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR)
           (MT-SC w)))

(define (single? w)
  (= (guessing-state-small w) (guessing-state-big w)))

(define (render-last-scene w)
  (overlay (text "End" SIZE COLOR) (MT-SC w)))

(define (start lower upper)
  (big-bang (guessing-state lower upper 0)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))
