#lang racket

(provide bon-appetit)
(require "shared.rkt" 2htdp/universe)

;; Constants
(define TICK .1)
(define PLAYER-LIMIT 2)
(define START-TIME 0)
(define WAIT-TIME 250)
(define FOOD*PLAYERS 5)

(define WEIGHT-FACTOR 2.1)
(define BASE-SPEED (/ (expt PLAYER-SIZE 2) WEIGHT-FACTOR))

(struct join (clients [time #:mutable]))
(struct play (players food spectators) #:mutable)
(define JOIN0 (join empty START-TIME))

(define-values
  (ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
  (let ()
    (struct ip (id iw body waypoints player))
    (define (create iw id body waypoints)
      (ip id iw body waypoints (player id body waypoints)))
    (values
      create ip? ip-id ip-iw ip-body ip-waypoints ip-player)))

(define (bon-appetit)
  (universe JOIN0
            (on-new connect)
            (on-tick tick-tock TICK)
            (on-msg handle-goto-message)
            (on-disconnect disconnect)))

(define (connect s iw)
  (cond [(join? s) (add-player s iw)]
        [(play? s) (add-spectator s iw)]))

(define (disconnect s iw)
  (cond [(join? s) (drop-client s iw)]
        [(play? s) (drop-player s iw)]))

(define (tick-tock s)
  (cond [(join? s) (wait-or-play s)]
        [(play? s) (move-and-eat s)]))

(define (move-and-eat pu)
  (define nplayers  (move-player* (play-players pu)))
  (define nfood (feed-em-all nplayers (play-food pu)))
  (progress nplayers nfood (play-spectators pu)))

(define (move-player* players)
  (for/list ([p players])
    (define waypoints (ip-waypoints p))
    (cond [(empty? waypoints) p]
          [else (define body  (ip-body p))
                (define nwpts (move-toward-waypoint body waypoints))
                (ip (ip-iw p) (ip-id p) body nwpts)])))

(define (move-toward-waypoint body waypoints)
  (define goal (first waypoints))
  (define bloc (body-loc body))
  (define line (- goal bloc))
  (define dist (magnitude line))
  (define speed (/ BASE-SPEED (body-size body)))
  (cond [(<= dist speed)
         (set-body-loc! body goal)
         (rest waypoints)]
        [else (> dist speed 0)
          (define velocity (/ (* speed line) dist))
          (set-body-loc! body (+ bloc velocity))
          waypoints]))

(define (feed-em-all players foods)
  (for/fold ([foods foods]) ([p players])
    (eat-all-the-things p foods)))

(define (eat-all-the-things player foods)
  (define b (ip-body player))
  (for/fold ([foods '()]) ([f foods])
            (cond
              [(body-collide? f b)
               (set-body-size! b (+ PLAYER-FATTEN-DELTA (body-size b)))
               foods]
              [else (cons f foods)])))

(define (body-collide? s1 s2)
  (<= (magnitude (- (body-loc s1) (body-loc s2)))
      (+ (body-size s1) (body-size s2))))

(define (handle-goto-message s iw msg)
  (cond [(and (play? s) (goto? msg)) (goto s iw msg)]
        [else (empty-bundle s)]))

(define (empty-bundle s)
  (make-bundle s empty empty))

(define (make-connection adder)
  (lambda (u iw)
    (define player (named-player iw))
    (define mails (list (make-mail iw (ip-id player))))
    (make-bundle (adder u player) mails empty)))

(define (join-add-player j new-p)
  (join (cons new-p (join-clients j)) (join-time j)))

(define add-player (make-connection join-add-player))

(define (named-player iw)
  (create-player iw (symbol->string (gensym (iworld-name iw)))))

(define (create-player iw n)
  (ip iw n (create-a-body PLAYER-SIZE) empty))

(define (drop-client j iw)
  (empty-bundle (join-remove j iw)))

(define (drop-player p iw)
  (broadcast-universe (play-remove p iw)))

(define (play-remove p iw)
  (define players (play-players p))
  (define spectators (play-spectators p))
  (play (rip iw players) (play-food p) (rip iw spectators)))

(define (join-remove j iw)
  (join (rip iw (join-clients j)) (join-time j)))

(define (rip iw players)
  (remove iw players (lambda (iw p) (iworld=? iw (ip-iw p)))))

;; The Join State and Tick Events

(define (wait-or-play j)
  (cond [(keep-waiting? j) (keep-waiting j)]
        [else (start-game j)]))

(define (keep-waiting? j)
  (or (> PLAYER-LIMIT (length (join-clients j)))
      (> WAIT-TIME (join-time j))))

(define (keep-waiting j)
  (set-join-time! j (+ (join-time j) 1))
  (time-broadcast j))

(define (time-broadcast j)
  (define iworlds (map ip-iw (join-clients j)))
  (define load% (min 1 (/ (join-time j) WAIT-TIME)))
  (make-bundle j (broadcast iworlds load%) empty))

(define (broadcast iws msg)
  (map (lambda (iw) (make-mail iw msg)) iws))

(define (bake-cupcakes player#)
  (for/list ([i (in-range (* player# FOOD*PLAYERS))])
            (create-a-body CUPCAKE)))

(define (broadcast-universe p)
  (define mails (broadcast (get-iws p) (serialize-universe p)))
  (make-bundle p mails empty))

(define (serialize-universe p)
  (define serialized-players (map ip-player (play-players p)))
  (list SERIALIZE serialized-players (play-food p)))

(define (get-iws p)
  (map ip-iw (append (play-players p) (play-spectators p))))

(define (create-a-body size)
  (define x (+ size (random (- WIDTH size))))
  (define y (+ size (random (- HEIGHT size))))
  (body size (make-rectangular x y)))

(define (play-add-spectator pu new-s)
  (define players (play-players pu))
  (define spectators (play-spectators pu))
  (play players (play-food pu) (cons new-s spectators)))

(define add-spectator (make-connection play-add-spectator))

(define (refresh players)
  (for/list ([p players])
    (create-player (ip-iw p) (ip-id p))))

(define (goto? msg)
  (and (list? msg)
       (= GOTO-LENGTH (length msg))
       (symbol? (first msg))
       (number? (second msg))
       (number? (third msg))
       (symbol=? GOTO (first msg))
       (<= 0 (second msg) WIDTH)
       (<= 0 (third msg) HEIGHT)))

(define (goto p iw msg)
  (define c (make-rectangular (second msg) (third msg)))
  (set-play-players! p (add-waypoint (play-players p) c iw))
  (broadcast-universe p))

(define (add-waypoint ps c iw)
  (for/list ([p ps])
            (cond [(iworld=? (ip-iw p) iw)
                  (ip (ip-iw p)
                      (ip-id p)
                      (ip-body p)
                      (append (ip-waypoints p) (list c)))]
            [else p])))

(define (progress pls foods spectators)
  (define p (play pls foods spectators))
  (cond [(empty? foods) (end-game-broadcast p)]
        [else (broadcast-universe p)]))

(define (end-game-broadcast p)
  (define iws (get-iws p))
  (define msg (list SCORE (score (play-players p))))
  (define mls (broadcast iws msg))
  (make-bundle (remake-join p) mls empty))

(define (score ps)
  (for/list ([p ps])
            (list (ip-id p) (get-score (body-size (ip-body p))))))

(define (remake-join p)
  (define players (refresh (play-players p)))
  (define spectators (play-spectators p))
  (join (append players spectators) START-TIME))

(define (start-game j)
  (define clients (join-clients j))
  (define cupcakes (bake-cupcakes (length clients)))
  (broadcast-universe (play clients cupcakes empty)))
