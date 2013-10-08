#lang racket

;; Predicates
(zero? 42)
(zero? 0)
(symbol=? 'a 'b)
(symbol=? 'a 'a)
(symbol? 'a)
(symbol? '(a b))
(struct student (name id# dorm))
(define student1 (student 'George 1234 'Durward))
(student? student1)
(boolean? (student? student1))
(empty? '())
(empty? student1)
(boolean=? true (student? student1))
(equal? true (student? student1))
(define (add-to-front-of-123 x)
  (cons x '(1 2 3)))
(list? (add-to-front-of-123 'a))
(cons? (add-to-front-of-123 'a))
(number? (add-to-front-of-123 'a))
(list? (add-to-front-of-123 '(a b c)))
(cons? (add-to-front-of-123 '(a b c)))

;;Conditionals: IF and Beyond
(if (student? student1)
    'student
    'not-student)
(if (student? 4.0)
    'student
    'not-student)
(if '(1)
      'everything-except-#f-counts-as-#t
      'aw-heck-no)
(if (student? 4.0)
      'everything-except-#f-counts-as-#t
      'aw-heck-no)
(if '()
      'everything-except-#f-counts-as-#t
      'aw-heck-no)
(define x 7)
(cond [(= x 7) 5]
      [(odd? x) 'odd-number]
      [else 'even-number])