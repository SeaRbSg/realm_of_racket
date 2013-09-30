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

(check-equal? 'yup
              (if (= (+ 1 2) 3)
                  'yup
                  'nope))

(check-equal? 'nope
              (if (= (+ 1 2) 4)
                  'yup
                  'nope))

(check-equal? 'everything-except-#f-counts-as-#t
              (if '(1)
                  'everything-except-#f-counts-as-#t
                  'aw-heck-no))

(check-equal? 'everything-except-#f-counts-as-#t
              (if empty
                  'everything-except-#f-counts-as-#t
                  'aw-heck-no))

(check-equal? 'aw-heck-no
              (if false
                  'everything-except-#f-counts-as-#t
                  'aw-heck-no))

(check-equal? 'odd-number
              (if (odd? 5)
                  'odd-number
                  'even-number))

(check-equal? 'odd-number
              (if (odd? 5)
                  'odd-number
                  (/ 1 0)))

(define x 7)

(check-equal? 5
              (if (even? x)
                  'even-number
                  (if (= x 7)
                      5
                      'odd-number)))

(check-equal? 5
              (cond [(= x 7)  5]
                    [(odd? x) 'odd-number]
                    [else     'even-number]))

(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length (rest a-list)))))

(check-equal? 4 (my-length '(list with four symbols)))
(check-equal? 1 (my-length '(42)))

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

(check-equal? '4-is-in
              (if (member 4 '(3 4 1 5))
                  '4-is-in
                  'not-in))

;; the book has a bug in it. They insist that this returns '(1 5)
;; when it returns '(4 1 5)
(check-equal? (member 4 '(3 4 1 5))
              '(4 1 5))

;; but then it goes to say that this is correct (which it is)
(define tasks '(1 clean 3 homework 4 party))
(check-equal? (member 3 tasks)
              '(3 homework 4 party))
