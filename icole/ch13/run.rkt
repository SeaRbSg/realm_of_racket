#lang racket
(require 2htdp/universe "client.rkt" "server.rkt")

(define (run)
  (launch-many-worlds (launch-guess-client "Adam")
                      (launch-guess-server)))