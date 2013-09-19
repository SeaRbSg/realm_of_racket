#lang racket

;; 4.1 How to Ask
(zero? 42)
(zero? 0)
(symbol=? 'a 'b)

(struct student (name id# dorm) #:transparent)

(define sophomore3 (student 'David 100234 'PG))
(student-name sophomore3)

(student? 'a)
(student? sophomore3)
(student? (student 1 2 3))
(student? "i am student")

(number? 'a)
(string? "hello world")
(symbol? 'a)
;;(image? 10)
(boolean? "false")

(list? 'eh)
(cons? '(what is that about))
(empty? 'a)

(real? 10)
(real? (sqrt -1))
(rational? 2/3)
(integer? 1.0)
(integer? 1)
(exact-integer? 1.0)

(= 1 2)
(= (sqrt -1) 0+1i)
(boolean=? #f #f)
(string=? "hello world" "good bye")
(equal? (student 'David 100234 'PG) sophomore3)

(equal? '(1 2 3) '(1 2 3))
(equal? 'a 'b)
(equal? "hello world" 'a)
(equal? 10 10)
(equal? #t 10)

(define (add-to-front-of-123 x)
  (cons x '(1 2 3)))

(add-to-front-of-123 'a)
(add-to-front-of-123 0)
(add-to-front-of-123 '(a b c))

;; 4.2
(if (= (+ 1 2) 3)
    'yup
    'nope)
(if (= (+ 1 2) 4)
    'yup
    'nope)

(if '(1)
    'everything-except-#f-counts-as-#t
    'aw-heck-no)
(if empty
    'everything-except-#f-counts-as-#t
    'aw-heck-no)
(if false
    'everything-except-#f-counts-as-#t
    'aw-heck-no)

(if (odd? 5) 'odd-number 'even-number)

(if (odd? 5) 'odd-number (/ 1 0))

(define x 7)
(if (even? x)
    'even-number
    (if (= x 7)
        5
        'odd-number))

(cond [(= x 7) 5]
      [(odd? x) 'odd-number]
      [else 'even-number])

(cond [(even? x) 'even-number]
      [(= x 7) 5]
      [else 'odd-number])

;; list-eater
(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length (rest a-list)))))
(my-length '(list with four symbols))
(my-length '(42))

;; 4.3 Cool Tricks with Conditionals
(define x1 5)
(define y 7)
(define z 9)
(and (odd? x1) (odd? y) (odd? z))

(define w 4)
(or (odd? w) (odd? y) (odd? z))

(define is-it-even #f)
is-it-even

(or (odd? x1) (set! is-it-even #t))
(and (even? x1) (set! is-it-even #t))
is-it-even

;;(if file-modified
;;    (if (ask-user-about-saving)
;;        (save-file)
;;        false)
;;    false)

;; (and file-modified (ask-user-about-saving) (save-file))

;; (when (and file-modified (ask-user-about-saving))
;;   (save-file))

;;(define filename "my-first-program.rkt")
;; (unless (ask-user-whether-to-keep-file filename)
;;   (delete-file filename))
(and (even? x) (set! is-it-even #t))