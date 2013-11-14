#lang racket

(provide lets-eat)

(require "shared.rkt"
         2htdp/universe 2htdp/image)

(struct app (id img countdown) #:transparent)
(struct entree (id players food) #:transparent)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shit not given in the book

(define FOOD-IMG (bitmap "cupcake.gif"))
(define PLAYER-IMG (bitmap "hungry-henry.gif"))
(define BASE (empty-scene WIDTH HEIGHT))
(define WAYPOINT-NODE (circle 3 'solid 'black))
(define PLAYER-COLOR "red")
(define MY-COLOR "blue")
(define WAYPOINT-COLOR "green")
(define LOADING... "Waiting For Server")
(define TEXT-SIZE 20)
(define TEXT-COLOR "black")
(define LOADING-OPEN-TEXT "\nYou are ")
(define SEPERATOR ": ")
(define PBAR-HEIGHT 35)
(define PBAR-LOC (- HEIGHT PBAR-HEIGHT))
(define PBAR-COLOR "red")
(define PBAR-TEXT (text "loading..." 20 "black"))
(define UPDATE-LENGTH 3)
(define END-LENGTH 2)
(define SCORE-LIST-LENGTH 2)
(define ZERO% 0)
(define LOADING (text LOADING... 20 "black"))
(define INITIAL (app #f LOADING ZERO%))
(define (update-entree s state-msg)
  (apply entree (entree-id s) (rest state-msg)))
(define (state? msg)
  (and (list? msg)
       (= UPDATE-LENGTH (length msg))
       (symbol? (first msg))
       (list? (second msg))
       (list? (third msg))
       (symbol=? SERIALIZE (first msg))
       (andmap player? (second msg))
       (andmap body? (third msg))))
(define (score? msg)
  (and (list? msg)
       (= END-LENGTH (length msg))
       (symbol? (first msg))
       (list? (second msg))
       (symbol=? SCORE (first msg))
       (score-list? (second msg))))
(define (score-list? l)
  (for/and ([s l])
    (and (list? s)
         (= SCORE-LIST-LENGTH (length s))
         (id? (first s))
         (number? (second s)))))
(define (add-progress-bar base count)
  (place-image (render-progress count) (/ WIDTH 2) PBAR-LOC base))
(define (render-progress count)
  (overlay PBAR-TEXT (rectangle (* count WIDTH) PBAR-HEIGHT "solid" PBAR-COLOR)))
(define (add-food foods base-scene)
  (for/fold ([scn base-scene]) ([f foods])
    (place-image FOOD-IMG (body-x f) (body-y f) scn)))
(define (render-player-score player)
  (render-text (number->string (get-score (body-size (player-body player))))))
(define (add-waypoint* player base-scene)
  (define loc  (body-loc (player-body player)))
  (define ways (player-waypoints player))
  (define-values (resulting-scene _)
    (for/fold ([scn base-scene][from loc]) ([to ways])
      (values (add-waypoint from to scn) to)))
  resulting-scene)
(define (add-waypoint from to s)
  (define x-from (real-part from))
  (define y-from (imag-part from))
  (define x-to (real-part to))
  (define y-to (imag-part to))
  (define with-line (add-line s x-to y-to x-from y-from WAYPOINT-COLOR))
  (place-image WAYPOINT-NODE x-to y-to with-line))
(define (get-text name-score)
  (define-values (name score) (apply values name-score))
  (string-append name SEPERATOR (number->string score)))
(define (render-text txt)
  (text txt TEXT-SIZE TEXT-COLOR))
(define (feaster-x feaster)
  (body-x (player-body feaster)))
(define (feaster-y feaster)
  (body-y (player-body feaster)))
(define (body-x body)
  (real-part (body-loc body)))
(define (body-y body)
  (imag-part (body-loc body)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 14.6: Main, Take Client

(define (lets-eat label server)
  (big-bang INITIAL
            (to-draw render-the-meal)
            (on-mouse set-waypoint)
            (on-receive handle-server-messages)
            (register server)
            (name label)))

(define (render-the-meal meal)
  (cond [(app? meal)    (render-appetizer meal)]
        [(entree? meal) (render-entree meal)]))

(define (handle-server-messages meal msg)
  (cond [(app? meal)    (handle-appetizer-message meal msg)]
        [(entree? meal) (handle-entree-message meal msg)]))

(define (set-waypoint meal x y me)
  (if (and (entree? meal) (mouse=? me "button-down"))
      (make-package meal (list GOTO x y))
      meal))

;; The Appetizer State

(define (render-appetizer app)
  (add-progress-bar (render-id+image app) (app-countdown app)))

(define (render-id+image app)
  (define id (app-id app))
  (define base-image (app-img app))
  (overlay (cond [(boolean? id) base-image]
                 [else (define s (string-append LOADING-OPEN-TEXT id))
                       (above base-image (text s TEXT-SIZE TEXT-COLOR))])
           BASE))

(define (handle-appetizer-message s msg)
  (cond [(id? msg) (app msg (app-img s) (app-countdown s))]
        [(time? msg) (app (app-id s) (app-img s) msg)]
        [(state? msg) (switch-to-entree s msg)]
        [else s]))

(define (time? msg)
  (and (real? msg) (<= 0 msg 1)))

(define (switch-to-entree s m)
  (apply entree (app-id s) (rest m)))

;; The Entree State

(define (render-entree entree)
  (define id (entree-id entree))
  (define pl (entree-players entree))
  (define fd (entree-food entree))
  (add-path id pl (add-players id pl (add-food fd BASE))))

(define (add-players id lof base-scene)
  (for/fold ([scn base-scene]) ([feaster lof])
    (place-image (render-avatar id feaster)
                 (feaster-x feaster) (feaster-y feaster)
                 scn)))

(define (render-avatar id player)
  (define size (body-size (player-body player)))
  (define color (if (id=? id (player-id player)) MY-COLOR PLAYER-COLOR))
  (above (render-text (player-id player))
         (overlay (render-player-score player)
                  PLAYER-IMG
                  (circle size 'outline color))))

(define (add-path id players base-scene)
  (define player (findf (lambda (x) (id=? id (player-id x))) players))
  (if (boolean? player)
      (base-scene)
      (add-waypoint* player base-scene)))

(define (handle-entree-message s msg)
  (cond [(state? msg) (update-entree s msg)]
        [(score? msg) (restart s msg)]
        [else s]))

(define (restart s end-msg)
  (define score-image (render-scores end-msg))
  (app (entree-id s) (above LOADING score-image) ZERO%))

(define (render-scores msg)
  (define scores (sort (second msg) < #:key second))
  (for/fold ([img empty-image]) ([name-score scores])
    (define txt (get-text name-score))
    (above (render-text txt) img)))
