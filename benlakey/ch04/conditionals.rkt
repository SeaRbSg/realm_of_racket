#lang racket
(if (= 1 2)
      'yup
      'nope)
(if (= 1 1)
      'yup
      'nope)
(if 'a
      'yup
      'nope)
(if empty
      'yup
      'nope)
(if false
      'yup
      'nope)
(if #f
      'yup
      'nope)

(equal? #f false)

(if (odd? 5)
      'yup
      'nope)

;; Doesnt blow up because else isnt evaluated
;; 'if' is a form not just a function
(if (odd? 5)
      'odd-number
      (/ 1 0))

(define x 7)
(if (even? x)
      'zipp
      (if (= x 7)
          'zapp
          'bloo))

(cond ((= x 7) 'seven)
        ((even? x) 'even)
        (else 'beep))

(cond (false 'one)
        (true 'two)
        (true 'three))

(define y 7)
(define z 9)
(and (odd? x) (odd? y) (odd? z))

(define a 2)
(or (odd? a) (odd? x))

(define foo 42)
(define bar 57)

;; Can use this to set defaults (similar notion to other languages)
(or (odd? foo) (set! foo bar))

(when #t 'yep)
(when #f 'yep)

(unless #t 'yep)
(unless #f 'yep)

(if (member 1 '(1 2 3))
      'yep
      'nope)
(member 1 '(1 2 3))


(struct point (x y) #:transparent)
(define (distance-to-origin p)
    (sqrt (+ (sqr (point-x p)) (sqr (point-y p)))))
(distance-to-origin (point 3 4))