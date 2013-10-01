#lang racket

(require rackunit)

;; pg 4.1

(check-false (zero? 42))
(check-true  (zero? 0))
(check-false (symbol=? 'a 'b))

(struct student (name id# dorm) #:transparent)

(define sophomore3 (student 'David 100234 'PG))

(check-equal? (student-name sophomore3)
              'David)

(check-false (student? 'a))
(check-true  (student? sophomore3))
(check-true  (student? (student 1 2 3)))
(check-false (student? "i am a student"))

(check-false (number? 'a))
(check-true  (string? "hello world"))
(check-true  (symbol? 'a))
(check-false (boolean? "false"))

;; (image? 10) ;; not actually part of racket, part of htdp?

(check-false (list? 'eh))
(check-true  (cons? '(what is that aboot?)))
(check-false (empty? 'a))

(check-true  (real? 10))
(check-false (real? (sqrt -1)))
(check-true  (rational? 2/3))
(check-true  (integer? 1.0))
(check-false (exact-integer? 1.0))

(check-false (= 1 2))
(check-true  (= (sqrt -1) 0+1i))
(check-true  (boolean=? #f #f))
(check-false (string=? "hello world" "goodbye"))
(check-true  (equal? (student 'David 100234 'PG) sophomore3))

(check-true  (equal? '(1 2 3) '(1 2 3)))
(check-false (equal? 'a 'b))
(check-false (equal? "hello world" 'a))
(check-true  (equal? 10 10))
(check-false (equal? #t 10))

(define (add-to-front-of-123 x)
  (cons x '(1 2 3)))

(check-equal? (add-to-front-of-123 'a)
              '(a 1 2 3))
(check-equal? (add-to-front-of-123 0)
              '(0 1 2 3))
(check-equal? (add-to-front-of-123 '(a b c))
              '((a b c) 1 2 3))

;; 4.2

(check-equal? (if (= (+ 1 2) 3)
                  'yup
                  'nope)
              'yup)

(check-equal? (if (= (+ 1 2) 4)
                  'yup
                  'nope)
              'nope)

(check-equal? (if '(1)
                  'everything-except-#f-counts-as-#t
                  'aw-heck-no)
              'everything-except-#f-counts-as-#t)

(check-equal? (if empty
                  'everything-except-#f-counts-as-#t
                  'aw-heck-no)
              'everything-except-#f-counts-as-#t)

(check-equal? (if false
                  'everything-except-#f-counts-as-#t
                  'aw-heck-no)
              'aw-heck-no)

(check-equal? (if (odd? 5)
                  'odd-number
                  'even-number)
              'odd-number)

(check-equal? (if (odd? 5)
                  'odd-number
                  (/ 1 0))
              'odd-number)

(define x 7)

(check-equal? (if (even? x)
                  'even-number
                  (if (= x 7)
                      5
                      'odd-number))
              5)

(check-equal? (cond [(= x 7)  5]
                    [(odd? x) 'odd-number]
                    [else     'even-number])
              5)

(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length (rest a-list)))))

(check-equal? (my-length '(list with four symbols))
              4)
(check-equal? (my-length '(42))
              1)

;; 4.3

(set! x 5) ; ugh
(define y 7)
(define z 9)

(check-true (and (odd? x) (odd? y) (odd? z)))

(define w 4)

(check-true (or (odd? w) (odd? y) (odd? z)))

(define is-it-even #f)
(check-false is-it-even)

(check-true  (or  (odd? x)  (set! is-it-even #t)))
(check-false (and (even? x) (set! is-it-even #t)))
(check-false is-it-even)

(check-pred void? (or  (odd? w) (set! is-it-even #t)))
(check-true is-it-even)

(check-false (and (odd? 5) (even? 5) (/ 1 0)))

(check-equal? (if (member 4 '(3 4 1 5))
                  '4-is-in
                  'not-in)
              '4-is-in)

;; the book has a bug in it. They insist that this returns '(1 5)
;; when it returns '(4 1 5)
(check-equal? (member 4 '(3 4 1 5))
              '(4 1 5))

;; but then it goes to say that this is correct (which it is)
(define tasks '(1 clean 3 homework 4 party))
(check-equal? (member 3 tasks)
              '(3 homework 4 party))

;; 4.4

(struct point (x y) #:transparent)

(define (distance-to-origin p)
  (sqrt (+ (sqr (point-x p))
           (sqr (point-y p)))))

(check-equal? (distance-to-origin (point 3 4))
              5)

(define pt1 (point -1 2))
(define pt2 (point -1 2))

(check-true (equal? pt1 pt2))

(check-false (eq? pt1 pt2))
(check-true  (eq? pt1 pt1))
(check-true  (eq? pt2 pt2))

(define pt3 pt1)

(check-true (eq? pt1 pt3))

(define (eq-first-items l1 l2)
  (eq? (first l1) (first l2)))

(check-true  (eq-first-items (cons pt1 empty) (cons pt3 empty)))

(check-false (eq-first-items (cons pt1 empty) (cons pt2 empty)))

;; consider 4.5 done since I've converted everything to tests.
