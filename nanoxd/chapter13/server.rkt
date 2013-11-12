#lang racket

;; GmNState is one of:
;; -(interval nat nat)
;; -#f

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
  (make-bundle w (list (make-mail client (guess w)))
               '()))

(define (next-interval u msg)
  (cond [(not (string? msg)) u]
        [(string=? "up" msg) (bigger u)]
        [(string=? "down" msg) (smaller u)]
        [else u]))

;; Game logic

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
