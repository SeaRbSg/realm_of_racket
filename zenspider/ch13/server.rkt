#lang racket

(require 2htdp/image 2htdp/universe "shared.rkt")

(provide launch-guess-server)

(struct interval (small big guesses))
(define u0 (interval LOWER UPPER 0))

(define (launch-guess-server)
  (universe false
            (on-new connect)
            (on-msg handle-msg)))

(define (connect u client)
  (if (false? u)
      (make-bundle u0 (list (make-mail client (guess u0))) empty)
      (make-bundle u empty (list client))))

(define (handle-msg u client msg)
  (define w (next-interval u msg))
  (make-bundle w (list (make-mail client (guess w))) empty))

(define (next-interval u msg)
  (cond [(not (string? msg)) u]
        [(string=? "up" msg) (bigger u)]
        [(string=? "down" msg) (smaller u)]
        [else u]))

;;; game logic:

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)
            (add1 (interval-guesses w))))

(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(define (single? w)
  (= (interval-small w) (interval-big w)))

(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))
            (add1 (interval-guesses w))))
