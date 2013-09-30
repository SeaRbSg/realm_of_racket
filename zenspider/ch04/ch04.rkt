#lang racket

(require rackunit)

;; pg 4.1

(check-false (zero? 42))
(check-true  (zero? 0))
(check-false (symbol=? 'a 'b))

(struct student (name id# dorm) #:transparent)

(define sophomore3 (student 'David 100234 'PG))

(check-equal? 'David (student-name sophomore3))

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

(check-equal? '(a 1 2 3)       (add-to-front-of-123 'a))
(check-equal? '(0 1 2 3)       (add-to-front-of-123 0))
(check-equal? '((a b c) 1 2 3) (add-to-front-of-123 '(a b c))0)

;; 4.2
