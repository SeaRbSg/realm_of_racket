#lang racket
;; 11.2 Lazy evaluation

(define lazy+ (lambda () (apply + (build-list 5000000 values))))

(define (make-lazy+ i)
  (lambda ()
    (apply + (build-list (* 500 i) values))))

(define long-big-list (build-list 5000 make-lazy+))
 
(define (compute-every-1000th l)
  (for/list ([thunk l] [i (in-naturals)]
                       #:when (zero? (remainder i 1000)))
    (thunk)))

;; 11.3 Memoized Compuations

(define (memoize suspended-c)
  (define hidden #f)
  (define run? #f)
  (lambda ()
    (cond [run? hidden]
          [else (set! hidden (suspended-c))
                (set! run? #t)
                hidden])))

(define mlazy+ (memoize lazy+))
(mlazy+)
(mlazy+)

(define (memoize.v2 suspended-c)
  (define (hidden)
    (define the-value (suspended-c))
    (set! hidden (lambda () the-value)) 
    the-value)
  (lambda () (hidden)))

;; 11.4 Racket Can Be Lazy
(define lazy+.v2 (delay (apply + (build-list 5000000 values))))
lazy+.v2
(force lazy+.v2)
(force lazy+.v2)