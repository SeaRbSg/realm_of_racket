#lang racket
(struct foo (bar) #:transparent)
(foo 5)
(struct foo2 (bar) #:prefab)
(foo2 5)