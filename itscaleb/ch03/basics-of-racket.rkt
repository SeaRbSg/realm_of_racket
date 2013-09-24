#lang racket
#| 
 Chapter 3 (Basics of Racket)
  - just experimenting with examples from the book
|#

;; Booleans
(zero? 1)
(zero? (sub1 1))
(symbol=? 'foo 'FoO)
(symbol=? 'foo 'foo)

;; Symbols
'foo!

;; Numbers

; imaginary
(sqrt -1)
; rational
(/ 4 6)

;; Strings
"tutti frutti"

;; CONS Cells
(cons 1 2)

(define cell (cons 'a 'b))

; extract left piece of data
(car cell)

;extract right piece of data
(cdr cell)


;; Lists

; the next two lines create the same list
(cons 'pork (cons 'beef empty))

;list is an easier alternative than cons
(list 'pork 'beef)

(define alist (list 'pork 'beef' chicken))
(first alist)
(rest alist)

;; Structures
; define the struct
(struct student (name id# dorm))

;create an instance
(define freshman1 (student 'Joe 1234 'NewHall))

;using accessors
(student-name freshman1)
