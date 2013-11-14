#lang racket

(provide lets-eat)

(require "shared.rkt" 2htdp/universe 2htdp/image)

;;
;; STRUCTS
;;

;; Appetizer state (really?)
; id - the player's server assigned id (false while waiting)
; img - the base image that displays messages to the screen
; countdown - time left until the game starts
(struct app (id img countdown))

;; Entree state (*sigh*)
; id - the player's server assigned id (false while waiting)
; players - list of current players
; food - list of available cupcakes
(struct entree (id players food))

;;
;; IMAGE CONSTANTS
;;
(define FOOD-IMG (bitmap "graphics/cupcake.gif"))
(define PLAYER-IMG (bitmap "graphics/hungry-henry.gif"))
(define BASE (empty-scene WIDTH HEIGHT))
(define WAYPOINT-NODE (circle 3 'solid 'black))
;; Color Constants
(define PLAYER-COLOR "red")
(define MY-COLOR "blue")
(define WAYPOINT-COLOR "green")
;; Text Constants
(define LOADING... "Waiting For Server")
(define TEXT-SIZE 20)
(define SCORE-SIZE 20)
(define TEXT-COLOR "black")
(define END-OPEN-TEXT "your score was: ")
(define END-CLOSE-TEXT ", the winner was player ")
(define LOADING-OPEN-TEXT "\nYou are ")
(define SEPERATOR ": ")
;; PBAR constants
(define PBAR-HEIGHT 35)
(define PBAR-LOC (- HEIGHT PBAR-HEIGHT))
(define PBAR-COLOR "red")
(define PBAR-TEXT (text "loading..." 20 "black"))
;; Message ID Constants
(define UPDATE-LENGTH 3)
(define SPLAYER-LENGTH 3)
(define SBODY-LENGTH 2)
(define END-LENGTH 2)
(define SCORE-LIST-LENGTH 2)
;; Init Constants
(define ZERO% 0)
(define LOADING (text LOADING... 20 "black"))
(define INITIAL (app #f LOADING ZERO%))

;; label is the name you wish to use for the game
;; server is the ip of the server to connect to (or LOCALHOST)
(define (lets-eat label server)
  (big-bang INITIAL
            (to-draw render-the-meal)
            (on-mouse set-waypoint)
            (on-receive handle-server-messages)
            (register server)
            (name label)))
;;
;; RENDERING
;;
(define (render-the-meal meal)
  (cond [(app? meal) (render-appetizer meal)]
        [(entree? meal) (render-entree meal)]))

(define (render-appetizer app)
  (add-progress-bar (render-id+image app)
                    (app-countdown app)))

(define (render-id+image app)
  (define id (app-id app))
  (define base-image (app-img app))
  (overlay
   (cond
     [(boolean? id) base-image]
     [else (define s (string-append LOADING-OPEN-TEXT id))
           (above base-image (text s TEXT-SIZE TEXT-COLOR))])
   BASE))

(define (render-entree entree)
  (define id (entree-id entree))
  (define pl (entree-players entree))
  (define fd (entree-food entree))
  (add-path id pl (add-players id pl (add-food fd BASE))))

(define (add-food foods base-scene)
  (for/fold ([scn base-scene]) ([f foods])
    (place-image FOOD-IMG (body-x f) (body-y f) scn)))

(define (add-players id lof base-scene)
  (for/fold ([scn base-scene]) ([feaster lof])
    (place-image (render-avatar id feaster)
                 (feaster-x feaster) (feaster-y feaster)
                 scn)))

(define (render-avatar id player)
  (define size (body-size (player-body player)))
  (define color
    (if (id=? id (player-id player))
        MY-COLOR
        PLAYER-COLOR))
  (above
   (render-text (player-id player))
   (overlay (render-player-score player)
            PLAYER-IMG
            (circle size 'outline color))))

(define (render-player-score player)
  (render-text (number->string (get-score (body-size (player-body player))))))

(define (add-path id players base-scene)
  (define player
    (findf (lambda (x) (id=? id (player-id x))) players))
  (if (boolean? player)
      base-scene
      (add-waypoint* player base-scene)))

(define (add-waypoint* player base-scene)
  (define loc  (body-loc (player-body player)))
  (define ways (player-waypoints player))
  (define-values (resulting-scene _)
    (for/fold ([scn base-scene][from loc]) ([to ways])
      (values (add-waypoint from to scn) to)))
  resulting-scene)

(define (render-scores msg)
  (define scores (sort (second msg) < #:key second))
  (for/fold ([img empty-image]) ([name-score scores])
    (define txt (get-text name-score))
    (above (render-text txt) img)))

(define (add-progress-bar base count)
  (place-image (render-progress count) (/ WIDTH 2) PBAR-LOC base))

(define (render-progress count)
  (overlay PBAR-TEXT (rectangle (* count WIDTH) PBAR-HEIGHT "solid" PBAR-COLOR)))

(define (render-text txt)
  (text txt TEXT-SIZE TEXT-COLOR))

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

;;
;; SERVER MESSAGE HANDLING
;;
(define (handle-server-messages meal msg)
  (cond [(app? meal) (handle-appetizer-message meal msg)]
        [(entree? meal) (handle-entree-message meal msg)]))

(define (handle-appetizer-message s msg)
  (cond [(id? msg) (app msg (app-img s) (app-countdown s))]
        [(time? msg) (app (app-id s) (app-img s) msg)]
        [(state? msg) (switch-to-entree s msg)]
        [else s]))

(define (handle-entree-message s msg)
  (cond [(state? msg) (update-entree s msg)]
        [(score? msg) (restart s msg)]
        [else s]))

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

(define (time? msg)
  (and (real? msg) (<= 0 msg 1)))

(define (switch-to-entree s m)
  (apply entree (app-id s) (rest m)))

(define (restart s end-msg)
  (define score-image (render-scores end-msg))
  (app (entree-id s) (above LOADING score-image) ZERO%))

;;
;; USER EVENT HANDLING
;;

(define (set-waypoint meal x y me)
  (if (and (entree? meal) (mouse=? me "button-down"))
      (make-package meal (list GOTO x y)) meal))

;;
;; OTHER
;;
(define (body-x body)
  (real-part (body-loc body)))

(define (body-y body)
  (imag-part (body-loc body)))

(define (feaster-x feaster)
  (body-x (player-body feaster)))

(define (feaster-y feaster)
  (body-y (player-body feaster)))
