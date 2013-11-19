#lang racket
(require 2htdp/image 2htdp/universe "shared.rkt")

(provide launch-guess-server)

(struct interval (small big))
(define u0 (interval LOWER UPPER))

(define (launch-guess-server)
  (universe #f
	    (on-new connect)
	    (on-msg handle-msg)))

(define (connect u client)
  (if (false? u)
    (make-bundle u0 (list (make-mail client (guess u0))) '())
    (make-bundle u empty (list client))))

(define (handle-msg u client msg)
  (define w (next-interval u msg))
  (make-bundle w (list (make-mail client (guess w))) '()))

(define (next-interval u msg)
  (cond [(not (string? msg)) u]
	[(string=? "up" msg) (bigger u)]
	[(string=? "down" msg) (smaller u)]
	[else u]))

(define (smaller u)
  (interval (interval-small u)
            (max (interval-small u)(sub1 (guess u)))))

(define (bigger u)
  (interval (min (interval-big u)(add1 (guess u)))
            (interval-big u)))

(define (guess u)
  (quotient (+ (interval-small u)(interval-big u)) 2))

(define (single? u)
  (= (interval-small u)(interval-big u)))
