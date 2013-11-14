#lang racket
(require 2htdp/universe)

(struct guess-server (clients interval))

(define i0 (interval LOWER UPPER))
(define player-count 0)

(provide launch-guess-server)

(define (launch-guess-server)
  (universe #f 
            (on-new connect)
            (on-msg handle-msg)
            (state true)))

;; On-new connection
;; See how many players I have
;; If you are the first player send mail saying set number
;; If you are the second player send mail saying guess number
;; Reject connection
(define (connect u client)
  
  
  
  )
   

(define (connect-setter client)
  (make-bundle 
   (guess-server 
      
