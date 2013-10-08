#lang racket
(require 2htdp/universe 2htdp/image)

(struct interval (small big))

(define HELP-TEXT-SIZE 11)
(define HELP-TEXT-COLOR "blue")

(define HELP-TEXT 
  (text "↑ for larger numbers, ↓ for smaller ones" 
        HELP-TEXT-SIZE 
        HELP-TEXT-COLOR))

(define HELP-TEXT2 
  (text "Press = when your number is guessed; q to quit." 
        HELP-TEXT-SIZE 
        HELP-TEXT-COLOR))

(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)

(define BIG-TEXT-COLOR "red")
(define BIG-TEXT-SIZE 72)

;; I grabbed these from the realm of racket repo since the ebook didnt talk about them.
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)

;; Holy bananas indention wonkyness. I'll have to ask about this.
;; Also, 'MT-SC' ? What kind of name is that?
(define BACKGROUND-SCENE 
  (place-image/align HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top" 
                     (place-image/align HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom" 
                                        (empty-scene WIDTH HEIGHT))))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

;; Define a function called 'start' to launch the big bang
;; 'lower' and 'upper' are supplied by the user
(define (start lower upper) 
  (big-bang (interval lower upper) ;; This first arg is the state
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

;; I didn't like how these methods took an argument called 'w' because that's not 
;; a very clear name. I renamed them to 'current-interval' because it seemed like 
;; a better name.
(define (smaller current-interval)
  (interval (interval-small current-interval)
            (max (interval-small current-interval) (sub1 (guess current-interval)))))

(define (bigger current-interval)
  (interval (min (interval-big current-interval) (add1 (guess current-interval)))
            (interval-big current-interval)))

(define (guess current-interval)
  (quotient (+ (interval-small current-interval) (interval-big current-interval)) 2))

(define (render current-interval)
  (overlay (text (number->string (guess current-interval)) BIG-TEXT-SIZE BIG-TEXT-COLOR) BACKGROUND-SCENE))

(define (render-last-scene current-interval)
  (overlay (text "End" BIG-TEXT-SIZE BIG-TEXT-COLOR) BACKGROUND-SCENE))

(define (single? current-interval)
  (= (interval-small current-interval) (interval-big current-interval)))