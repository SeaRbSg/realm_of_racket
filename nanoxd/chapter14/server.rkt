#lang racket

(struct join (clients [time #:mutable]))
(struct play (players food spectators) #:mutable)

(define-values
  (ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
  (let ()
    (struct ip (id iw body waypoints player))
    (define (create iw id body waypoints)
      (ip id iw body waypoints (player id body waypoints)))
    (values
      create ip? ip-id ip-iw ip-body ip-waypoints ip-player)))
