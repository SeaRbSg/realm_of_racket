#lang racket
(require 2htdp/universe 2htdp/image)

;; Stop! It's the motherfuckin' remix.
(struct guessing-state (low high guess-count))

(define TEXT-SIZE 12)
(define TEXT-X 3)
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
(define SIZE 100)
(define COUNT-SIZE 36)

(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define TEXT-MIDDLE-Y 67)

(define (smaller w)
  (guessing-state (guessing-state-low w)
            (max (guessing-state-low w) (sub1 (guess w)))
            (add1 (guessing-state-guess-count w))))

(define (bigger w)
  (guessing-state (min (guessing-state-high w) (add1 (guess w)))
            (guessing-state-high w)
            (add1 (guessing-state-guess-count w))))

(define (guess w)
  (quotient (+ (guessing-state-low w) (guessing-state-high w)) 2))


(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR)
           (MT-SC w)))

(define (single? w)
  (= (guessing-state-low w) (guessing-state-high w)))

(define (render-last-scene w)
  (overlay (text "FIN" SIZE COLOR) (MT-SC w)))

(define (MT-SC w)
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   ;; http://docs.racket-lang.org/reference/generic-numbers.html for num-conversion
   (place-image/align
    (text (number->string (guessing-state-guess-count w))
          COUNT-SIZE COLOR) TEXT-X TEXT-MIDDLE-Y "left" "middle"
    (place-image/align
     HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
     (empty-scene WIDTH HEIGHT)))))

;; Vim keys FTW
(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "k") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "j") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (start lower upper)
  (big-bang (guessing-state lower upper 0)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))
