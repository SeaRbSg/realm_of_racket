#lang racket

(require 2htdp/universe "server.rkt" "client.rkt")

(define (run)
  (launch-many-worlds (launch-guess-client "Fernando")
                      (launch-guess-server)))
