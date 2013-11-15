#lang racket

(require (only-in "server.rkt" bon-appetit)
         (only-in "client.rkt" lets-eat)
         2htdp/universe)

(define (server-dinner)
  (launch-many-worlds (bon-appetit)
                      (lets-eat "Fernando" LOCALHOST)
                      (lets-eat "Daniel" LOCALHOST)))
