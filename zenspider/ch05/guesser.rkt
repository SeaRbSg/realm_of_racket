#lang racket

(require 2htdp/universe 2htdp/image)

(struct interval (small big))

(define HEIGHT       480)
(define SIZE         72)
(define TEXT-SIZE    12)
(define TEXT-LOWER-Y (- HEIGHT TEXT-SIZE -10))
(define TEXT-UPPER-Y 10)
(define TEXT-X       3)
(define WIDTH        640)
(define COLOR        "red")

(define HELP-TEXT
  (text "UP for larger numbers, DOWN for smaller ones"
        TEXT-SIZE
        "blue"))

(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))

(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))

(define (deal-with-guess w key)
  (cond [(key=? key "up")   (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q")    (stop-with w)]
        [(key=? key "=")    (stop-with w)]
        [else w]))

(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC))

(define (render-last-scene w)
  (overlay (text "End" SIZE COLOR) MT-SC))

(define (single? w)
  (= (interval-small w) (interval-big w)))

(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

(define (start lower upper)
  (big-bang (interval lower upper)
            [on-key    deal-with-guess]
            [to-draw   render]
            [stop-when single? render-last-scene]))

(start 1 100)
