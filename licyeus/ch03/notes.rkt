#lang racket

; making lists
(list 'duck 'duck)
'(duck duck)
(cons 'duck 'duck)
(cons 'duck empty)

; list is just a bunch of consing
'(duck duck goose)
(cons 'duck (cons 'duck (cons 'goose empty)))
(equal? '(duck duck goose) (cons 'duck (cons 'duck (cons 'goose empty))))

; nested lists and list utility functions
'(duck duck goose)
(first '(duck duck goose))
(rest '((duck duck) (goose goose goose) rabbit))
(second '((duck duck) (goose goose goose) rabbit))

; structs
(struct student (name id# dorm))
(define andrew (student 'Andrew 3141 'SagePoint))
(define melisa (student 'Melisa 1234 'NewHall))
(define in-class (list andrew melisa))
(student-name (last in-class))

; nested structs
(struct student-body (undergrads grads))
(define all-students
  (student-body (list melisa)
                (list andrew)))
(student-name (first (student-body-grads all-students)))

; struct transparency
(struct example (p q r) #:transparent)
(define ex (example 3 1 4))
ex