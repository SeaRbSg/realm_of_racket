#lang racket
(provide foo addone)
(define foo 5)
(define (addone) (set! foo (addx 1)))
(define (addx x) (+ foo x))