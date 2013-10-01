#lang racket
#|
 Chapter 4.5 (define define 'define)
|#

;; Module-Level Definitions
; can be accessed from anywhere inside a module

; Variable definition
(define MAXHEIGHT 100)

; Function definition
(define (five)
  5)
(five)

;; Local Definitions
; Variable of function definitions that can only be accessed 
; from inside of the functions or conds they are defined in
(define (six)
  (define local_var 6) ; 'local_var' is local to this function
  local_var)
(six)