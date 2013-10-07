#lang racket
(define (my-length a-list)
    (if (empty? a-list)
        0
        (add1 (my-length (rest a-list)))))