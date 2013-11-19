#lang racket

;; Chapter 11

;; 11.2 Lazy Evaluation

; Easy way to create a thunk: return a no-arguments lambda wrapped around an expression.

(define lazy+
  (lambda ()
    (apply + (range 5000000))))

; you can create functions that create thunks by returning a lambda that wraps a given expression.

(define (make-lazy+ i)
  (lambda ()
    (apply + (range (* 500 i)))))

(define long-big-list
  (build-list 5000 make-lazy+)) ; a list of 5000 thunks not yet executed.

(define (compute-every-1000th l)
  (for/list ([thunk l]
             [i (in-naturals)]
             #:when (zero? (remainder i 1000)))
    (thunk))) ; executes every 1000th thunk in a list and returns a list of the computed values.

; 11.3 Memoized Computations

(define (memoize thunk) ; first attempt at function that memoizes a thunk
  (define hidden #f)
  (define run? #f)
  (lambda ()
    (cond [run? hidden]
          [else (set! hidden (thunk))
                (set! run? #t)
                hidden])))

(define mlazy+
  (memoize (lambda ()
             (apply + (range 1000000)))))

(mlazy+) ; first call takes a second while computing value
(mlazy+) ; subsequent calls return the computed value

; second attempt at memoize function
(define (memoize.v2 thunk)
  (define (hidden)
    (define the-value (thunk))
    (set! hidden (lambda ()
                        the-value))
    the-value)
  (lambda ()
    (hidden)))

; there is also 'delay' and 'force'

(define lazy++
  (delay (apply + (range 1000000)))) ; delay returns a promise. a promise created by delay is also memoized

(force lazy++) ; force on a promise makes it run. 

