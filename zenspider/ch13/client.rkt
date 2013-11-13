#lang racket

(require 2htdp/image 2htdp/universe "shared.rkt")

(provide launch-guess-client launch-guess-client2)

(struct interval (small big) #:prefab)

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

(define clientstate1 "provide a number")

(struct game (interval guess) #:transparent)

(define (launch-guess-client n)    ; answerer
  (big-bang "answerer"
            (to-draw draw-answer)
            (on-key handle-keys1)
            (name n)
            (register LOCALHOST)
            (on-receive handle-msg1)))

(define (launch-guess-client2 n)    ; guesser
  (big-bang "guesser"
            (to-draw draw-guess)
            (on-key handle-keys2)
            (name n)
            (register LOCALHOST)
            (on-receive handle-msg2)))

(define (handle-msg1 w msg) ; answerer
  (cond [(number? msg) msg]
        [else (raise (format "bad msg1: ~s for ~s" msg w))]))

(define (handle-msg2 w msg) ; guesser
  (cond [(interval? msg) (game msg (next-guess msg))]
        [(symbol? msg)
         (let* ((i     (game-interval w))
                (guess (game-guess w))
                (big   (interval-big i))
                (small (interval-small i)))
           (case msg
             [(<) (game (interval small guess) guess)]
             [(>) (game (interval guess big)   guess)]
             [(=) (game (interval guess guess) guess)]
             [else (raise (format "unhandled msg2: ~s for ~s" msg w))]))]
        [else (raise (format "bad msg2: ~s for ~s" msg w))]))

(define (handle-keys1 w key) ; answerer
  (case key
    [("up" "<")   (make-package w '>)]
    [("down" ">") (make-package w '<)]
    [("=")        (make-package w '=)]
    [else   (raise (format "unknown key: ~s for ~s" key w))]))

(define (handle-keys2 w key) ; guesser
  (define (new-game w f) (game (game-interval w) (f (game-guess w))))
  (case key
    [("up")    (new-game w add1)]
    [("down")  (new-game w sub1)]
    [("=")     (new-game w values)]
    [("\r")    (make-package w (game-guess w))]
    [("shift") w]
    [else (raise (format "unknown key: ~s for ~s" key w))]))

(define (draw-guess w)
  (if (string? w)
      (overlay (text w SIZE COLOR) MT-SC)
      (overlay (text (format "~s..~s = ~s"
                             (interval-small (game-interval w))
                             (interval-big   (game-interval w))
                             (game-guess w)) 24 COLOR) MT-SC)))

(define (draw-answer w)
  (overlay (text (format "User guessed ~s" w) SIZE COLOR) MT-SC))

;;; game logic

(define (next-guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(module+ test
  (require rackunit)
  (require 2htdp/universe)

  (define i0 (interval 0 100))
  (define i1 (interval 0 42))
  (define i2 (interval 42 100))
  (define i3 (interval 42 42))
  (define w0 (game i0 42))

  ;; 3) server sends (interval 0 100) to iworld2
  ;; 4) iworld2 records the interval
  (check-equal? (handle-msg2 w0 i0)
                (game i0 50))

  ;; 5) iworld2 player makes a guess, sends back 42
  (check-equal? (handle-keys2 w0 "up")
                (game i0 43))
  (check-equal? (handle-keys2 w0 "down")
                (game i0 41))
  (check-equal? (handle-keys2 w0 "\r")
                (make-package w0 42))

  ;; 7) iworld1 receives guess, stores it for user consideration
  (check-equal? (handle-msg1 iworld1 42)
                42)

  ;; 8) iworld2 user responds to guess, sends result back to server
  (check-equal? (handle-keys1 42 "down")
                (make-package 42 '<))
  (check-equal? (handle-keys1 42 "up")
                (make-package 42 '>))
  (check-equal? (handle-keys1 42 "=")
                (make-package 42 '=))

  ;; 10) server sends response from iworld1 to iworld2

  (check-equal? (handle-msg2 w0 '<)
                (game i1 42))
  (check-equal? (handle-msg2 w0 '>)
                (game i2 42))
  (check-equal? (handle-msg2 w0 '=)
                (game i3 42)))
