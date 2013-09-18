#lang racket

;; determine or set the upper and lower limits of the player's number

(define lower 1)
(define upper 100)

(define (start n m)
  (set! lower (min n m))
  (set! upper (max n m))
  (guess))

;; guess a number halfway between those two numbers

(define (guess)
  (quotient (+ lower upper) 2))

;; if the player says the number is smaller, lower the upper limit

(define (smaller)
  (set! upper (max lower (sub1 (guess))))
  (guess))

;; if the player says the number is larger, raise the lower limit

(define (larger)
  (set! lower (min upper (add1 (guess))))
  (guess))
