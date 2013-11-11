#lang racket

(define clientstate0 "none available")

(define (launch-guess-client n)
  (big-bang clientstate0
            (to-draw draw-guess)
            (on-key handle-keys)
            (name n)
            (register LOCALHOST)
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
