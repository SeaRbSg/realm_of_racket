#lang racket

(require 2htdp/universe "server.rkt" "client.rkt")

(define (run)
  (launch-many-worlds (launch-guess-client "Ryan")
                      (launch-guess-server)))

(define (run2)
  (launch-many-worlds (launch-guess-client "Ryan")
                      (launch-guess-client2 "Fred")
                      (launch-guess-server)))
