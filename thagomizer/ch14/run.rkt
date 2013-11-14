#lang racket

(require (only-in "server.rkt" bon-appetit)
         (only-in "client.rkt" lets-eat)
         2htdp/universe)

(define (serve-dinner)
  (launch-many-worlds
   (bon-appetit)
   (lets-eat "Gizmo" LOCALHOST)
   (lets-eat "Nick" LOCALHOST)))
