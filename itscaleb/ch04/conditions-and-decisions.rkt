#lang racket

;; Predicates

; structure definitions define a type predicate funtion.
(struct student (name id#) #:transparent)
(define student1 (student 'John 12345))
(student? student1)

; built in data also has a type predicate
(number? 1)
(string? "hello")
(real? (sqrt -1))

; use equality predicates to test equality
(string=? "hello" "hello")
(= 1 2)

(define stu1 (student 'john 12345))
(define stu2 (student 'john 12345))

; question => if a structure isn't declared with '#:transparent', the following is false
(equal? stu1 stu2)

;; Conditionals

; note => #f is false, and anything not false is true

(if (= (+ 1 2) 3)
    'yup
    'nope)

; note => usually all the parameters are executed before the method. Not in an if statement.

(if (zero? 0)
    (/ 1 1)
    (/ 1 0) ) ; <- div by zero not executed

;; Cond
(define x 7)
(cond [(= x 7) 5]
      [(odd? x) 'odd-number]
      [else 'even-number])

;; Recursion

; example of a 'list eating' function
(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length(rest a-list)))))

(define a-list '(1 2 3))
(my-length a-list)

; and
(and (odd? 1) (odd? 3) (odd? 5))
; or
(or (odd? 1) (odd? 2))

; note => 'and' and 'or' evaluate expressions, so you can use them as conditionals.
(define num 2)
(and (even? num) (set! num 1)) ; "if num is even, set num to 1
num

; considered better is to use 'when' because it seperates the condition from the action.
(when (odd? num)
  (set! num 2))
num
;question => is it common to use 'and'  and 'or' as conditionals?

; reference equality
(struct point(x y))
(define p1 (point 1 2))
(define p2 p1)
(eq? p1 p2)