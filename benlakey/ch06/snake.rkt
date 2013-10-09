#lang racket
(struct pit (snake goos) #:transparent)
(struct snake (direction segments) #:transparent)
(struct point (x y) #:transparent)
(struct goo (location expiration-ticks) #:transparent)
