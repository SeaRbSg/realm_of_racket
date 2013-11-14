#lang racket
(require 2htdp/image 2htdp/universe)

(struct world (role data))

(provide launch-guess-client)

(define clientstate0 "none available")

(define (launch-guess-client n)
  (big-bang clientstate0
            (to-draw render)
            (on-key handle-keys)
            (name n)
            (register LOCALHOST)
            (on-receive handle-msg)
            (state true)))

(define (handle-msg client msg)
  (cond [(eq? msg "guesser")
         (world msg '())]
        [(eq? msg "setter")
         (world msg '())]
        
        ))

(define (handle-keys w key)
  
