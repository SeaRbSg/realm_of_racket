#lang racket

(require 2htdp/image 2htdp/universe "shared.rkt")

(provide launch-guess-server)

(define (launch-guess-server)
  (universe false
            (on-new connect)
            (on-msg handle-msg)))

(define (add-world u w)
  (let ((u+ (append u (list w))))
    (make-bundle u+ (list (make-mail w `#s(interval ,LOWER ,UPPER))) empty)))

(define (connect u w)
  (if (false? u)
      (make-bundle (list w) empty empty)
      (add-world u w)))

(define (pass-through u client msg)
  (define other (if (equal? (first u) client) second first))
  (make-bundle u (list (make-mail (other u) msg)) empty))

(define (handle-msg u client msg)
  (cond [(number? msg) (pass-through u client msg)]
        [(equal? '= msg) (stop-with "done!")]
        [(symbol? msg) (pass-through u client msg)]
        [else
         (raise (format "bad msg: ~s from ~s" msg (iworld-name client)))]))

;;; game logic:

(module+ test
  (require rackunit)
  (require 2htdp/universe)

  (define g1 (list iworld1))
  (define g2 (list iworld1 iworld2))

  ;; c1                         svr                     c2
  ;;  --------connect-------->
  ;;                               <--------connect------
  ;;                               ----(interval 0 100)->
  ;;                                         (interval 0 100)
  ;;                                        user inputs guess
  ;;                               <-----------42--------
  ;;   <---------------42-----
  ;;    -----------'<------->
  ;;                               ----------'<--------->
  ;;                                         (interval 0 42)
  ;;                                       user inputs guess
  ;;                               <-----------36--------
  ;;   <---------------36-----
  ;;    -----------'<------->
  ;;                               ----------'<--------->
  ;;                                         (interval 0 36)
  ;;                                       user inputs guess
  ;;                               <-----------14--------
  ;;   <---------------14-----
  ;;    ------------'>------->
  ;;                               -----------'>--------->
  ;;                                         (interval 14 36)
  ;;                                       user inputs guess
  ;;                               <-----------25--------
  ;;   <---------------25-----
  ;;    -----------'=-------->
  ;;                               ----------'=---------->


  ;; 1) first client connects, no response
  (check-equal? (connect false iworld1)
                (make-bundle g1
                             empty
                             empty))

  ;; 2) second client connects, tell it to go
  (check-equal? (connect g1 iworld2)
                (make-bundle g2
                             (list (make-mail iworld2 #s(interval 0 100)))
                             empty))

  ;; 6) server receives guess, passes to iworld1
  (check-equal? (handle-msg g2 iworld2 42)
                (make-bundle g2
                             (list (make-mail iworld1 42))
                             empty))

  ;; 9) server receives '< and sends '< to iworld2
  (check-equal? (handle-msg g2 iworld1 '<)
                (make-bundle g2
                             (list (make-mail iworld2 '<))
                             empty))

  )
