#lang racket

(require rackunit)

;; ch 4 1/2

(define WIDTH 100)
(define HEIGHT 200)

(define X-CENTER (quotient WIDTH 2))
(define Y-CENTER (quotient HEIGHT 2))

(unless (> HEIGHT 0)
  (error 'guess-my-number "HEIGHT may not be negative"))

(define SQR-COLOR "red")
(define SQR-SIZE 10)

;; (define (draw-square img x y)
;;   (place-image (square SQR-SIZE "solid" SQR-COLOR) xy
;;                img))

(struct posn (x y))
(struct rectangle (width height))
(define (inside-of-rectangle? r p)
  (define x (posn-x p))
  (define y (posn-y p))
  (define width (rectangle-width r))
  (define height (rectangle-height r))
  (and (<= 0 x) (< x width) (<= 0 y) (< y height)))

;; (define (random-stars n)
;;   (cond [(zero? n) '()]
;;         [else (define location (random-location 200 300))
;;               (if (inside-moon? location)
;;                   (random-stars n)
;;                   (cons location (random-stars (sub1 n))))]))

(struct record (name score))

(define (winning-players lst)
  (define sorted-lst (sort lst))
  (define (winners lst pred)
    (cond
     [(empty? lst) (list pred)]
     [else
      (define fst (first lst))
      (if (> (record-score pred) (record-score fst))
          (list pred)
          (cons pred (winners (rest lst) fst)))]))
  ;; START HERE:
  (winners (rest sorted-lst) (first sorted-lst)))
