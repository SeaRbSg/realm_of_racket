#lang racket

(require (only-in "server.rkt" bon-appetit)
         (only-in "client.rkt" lets-eat)
         2htdp/universe)

(define (serve-dinner)
  (launch-many-worlds (bon-appetit)
                      (lets-eat "Ryan" LOCALHOST)
                      (lets-eat "Aja"  LOCALHOST)))
(module+ main
  (serve-dinner))