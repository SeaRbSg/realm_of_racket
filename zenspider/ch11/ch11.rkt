#lang racket

(require rackunit)

;;; 11.2: Lazy Evaluation

(define lazy+
  (lambda ()
    (apply + (build-list 5000000 values))))

(define (make-lazy+ i)
  (lambda ()
    (apply + (build-list (* 500 i) values))))

(define long-big-list (build-list 5000 make-lazy+))

(define (compute-every-1000th l)
  (for/list ([thunk l]
             [i (in-naturals)]
             #:when (zero? (remainder i 1000)))
    (thunk)))

(define (triangle-number n)
  (/ (+ (* n n) n) 2))

(check-equal? (compute-every-1000th long-big-list)
              (list 0
                    (triangle-number (sub1 (* 500 1000)))
                    (triangle-number (sub1 (* 500 2000)))
                    (triangle-number (sub1 (* 500 3000)))
                    (triangle-number (sub1 (* 500 4000)))))

;;; 11.3: Memoized Computations

(define (memoize suspended-c)
  (define hidden false)
  (define run? false)
  (lambda ()
    (cond [run? hidden]
          [else (set! hidden (suspended-c))
                (set! run? true)
                hidden])))

;; from above:
;; (define lazy+ (lambda () (apply + (build-list 5000000 values))))

(define mlazy+ (memoize lazy+))

(check-equal? (mlazy+)
              (triangle-number (sub1 5000000)))
(check-equal? (mlazy+)
              (triangle-number (sub1 5000000)))

(define (memoize.v2 suspended-c)
  (define (hidden)
    (define the-value (suspended-c))
    (set! hidden (lambda () the-value))
    the-value)
  (lambda () (hidden)))

(define mlazy+2 (memoize lazy+))

(check-equal? (mlazy+2)
              (triangle-number (sub1 5000000)))
(check-equal? (mlazy+2)
              (triangle-number (sub1 5000000)))

(define lazy++ (delay (apply + (build-list 5000000 values))))

(check-equal? (force lazy++)
              (triangle-number (sub1 5000000)))
(check-equal? (force lazy++)
              (triangle-number (sub1 5000000)))
