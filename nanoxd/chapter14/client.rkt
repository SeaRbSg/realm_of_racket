#lang racket

(require "shared.rkt"
         2htdp/universe 2htdp/image)

(struct app (id img countdown) #:transparent)
(struct entree (id players food) #:transparent)

;; 14.6 Main, Take Client

(define (lets-eat label server)
  (big-bang INITIAL
            (to-draw render-the-meal)
            (on-mouse set-waypoint)
            (on-receive handle-server-messages)
            (register server)
            (name label)))

(define (render-the-meal meal)
  (cond [(app? meal) (render-appetizer meal)]
        [(entree? meal) (render-entree meal)]))

(define (handle-server-messages meal msg)
  (cond [(app? meal) (handle-appetizer-message meal msg)]
        [(entree? meal) (handle-entree-message meal msg)]))

(define (set-waypoint meal x y me)
  (if (and (entree? meal) (mouse=? me "button-down"))
    (make-package meal (list GOTO x y))
    meal))

(define (render-appetizer app)
  (add-progress-bar (render-id+image app) (app-countdown app)))

(define (render-id+image app)
  (define id (app-id app))
  (define base-image (app-img app))
  (overlay
    (cond
      [(boolean? id) base-image]
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
  (define color (if (id=? id (player-id player)) MY-COLOR
                  PLAYER-COLOR))
  (above
    (render-text (player-id player))
    (overlay (render-player-score player)
             PLAYER-IMG
             (circle size 'outline color))))

(define (add-path id players base-scene)
  (define player (findf (lambda (x) (id=? id (player-id x))) players))
  (if (boolean? player)
    base-scene
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
