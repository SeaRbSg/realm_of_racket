#lang racket
(provide (struct-out bar) (struct-out baz))
(struct bar (a b))
(struct baz (a [b #:mutable]))