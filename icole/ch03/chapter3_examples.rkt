#lang racket

;; Booleans
(zero? 1)
(zero? (sub1 1))

;; Symbols
(symbol=? 'foo 'FoO)
(symbol=? 'foo 'foo)

;; Numbers
(expt 2 3)
(expt 53 53)
(sqrt -1)
(/ 5 10)
(/ 5.0 10)

;; Strings
"tutti frutti"
(string-append "tutti" "frutti")
(string-append "tutti" " " "frutti")

;; CONS Cells
(cons 1 2)
(define cell (cons 'a 'b))
(car cell)
(cdr cell)

(cons 'chicken empty)
(cons 'chicken '())

(cons 'pork '(beef chicken))
(cons 'beef (cons 'chicken '()))
(cons 'pork (cons 'beef (cons 'chicken '())))

;; LIST Function
(list 'pork 'beef 'chicken)

;; FIRST and REST Function
(first (cons 'pork (cons 'beef (cons 'chicken '()))))
(rest (cons 'pork (cons 'beef (cons 'chicken '()))))
(first (rest (cons 'pork (cons 'beef (cons 'chicken '())))))

;; Nested Lists
(list 'cat (list 'duck 'bat) 'ant)
(first '((peas carrots tomatoes) (pork beef chicken)))
(rest '((peas carrots tomatoes) (pork beef chicken)))
(second '((duck bat) ant))

;; Structures
(struct student (name id# dorm))
(define freshman1 (student 'Joe 1234 'NewHall))
(student-name freshman1)
(student-id# freshman1)
;; Define a number of students
(define mimi (student 'Mimi 1234 'NewHall))
(define nicole (student 'Nicole 5678 'NewHall))
(define rose (student 'Rose 8765 'NewHall))
(define eric (student 'Eric 4321 'NewHall))
;;List of structs
(define in-class (list mimi nicole rose eric))
;;Struct to represent student body w/ years
(struct student-body (freshmen sophomores juniors seniors))
(define all-students
  (student-body (list freshman1 mimi)
                (list nicole)
                (list rose eric)
                empty))

;;Interact with defined all-students struct
(student-name (second (student-body-freshmen all-students)))
(student-name (first (student-body-juniors all-students)))

;;Structure Transparency
(struct example (x y z) #:transparent)
(define ex1 (example 1 2 3))
ex1