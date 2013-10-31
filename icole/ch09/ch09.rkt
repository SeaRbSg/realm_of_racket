#lang racket
;;Chapter 9 examples

(for ([i '(12345) ])
  (display i))

(for/list ([i '(12345) ])
  (/ 1 i))