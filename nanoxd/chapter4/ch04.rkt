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

(if (= (+ 1 2) 3)
    'yup
    'nope)
(if (= (+ 1 2) 4)
    'yup
    'nope)

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

(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length (rest a-list)))))
(my-length '(list with four symbols))
(my-length '(42))

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

(if (member 4 (list 3 4 1 5)) '4-is-in 'not-in)
(member 1 '(3 4 1 5))

(define tasks '(1 clean 3 homework 4 party))
(member 3 tasks)

(struct point (x y) #:transparent)
(define (distance-to-origin p)
  (sqrt (+ (sqr (point-x p)) (sqr (point-y p)))))
(distance-to-origin (point 3 4))
(distance-to-origin (point 12 5))

(define pt1 (point -1 2))
(define pt2 (point -1 2))
(equal? pt1 pt2)

(eq? pt1 pt2)
(eq? pt1 pt1)
(eq? pt2 pt2)

;; equal? is value equality. eq? is object-id equality.

(define pt3 pt1)
(eq? pt1 pt3)

(define (eq-first-items list1 list2)
  (eq? (first list1) (first list2)))

(eq-first-items (cons pt1 empty) (cons pt3 empty))
(eq-first-items (cons pt1 empty) (cons pt2 empty))
