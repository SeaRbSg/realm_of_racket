#lang racket

(provide bon-appetit)

(require "shared.rkt"
         2htdp/universe)

(struct join (clients [time #:mutable]) #:transparent)
(struct play (players food spectators generate) #:mutable #:transparent)
;; HATE: play-players isn't players, it's ips. wtf?
(define-values (ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
  (let ()
    (struct ip (id iw body waypoints player))
    (define (create iw id body waypoints)
      (ip id iw body waypoints (player id body waypoints)))
    (values create ip? ip-id ip-iw ip-body ip-waypoints ip-player)))

;; Shit they don't give me in the book

(define TICK .1)
(define PLAYER-LIMIT 2)
(define START-TIME 0)
(define WAIT-TIME 25)
(define FOOD*PLAYERS 5)
(define WEIGHT-FACTOR 2.1)
(define BASE-SPEED (/ (expt PLAYER-SIZE 2) WEIGHT-FACTOR))
(define (play-add-spectator pu new-s)
  (define players (play-players pu))
  (define spectators (play-spectators pu))
  (define generate (play-generate pu))
  (play players (play-food pu) (cons new-s spectators) generate))
(define (play-remove p iw)
  (define players (play-players p))
  (define spectators (play-spectators p))
  (define generate (play-generate p))
  (play (rip iw players) (play-food p) (rip iw spectators) generate))
(define JOIN0 (join empty START-TIME))
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
(define (broadcast-universe p)
  ;; TODO: try to get this back to broadcast... I don't have high hopes...
  (define (mail-player pl) (make-mail (ip-iw pl) (serialize-universe p (ip-id pl))))
  (define mails (map mail-player (append (play-players p) (play-spectators p))))
  (make-bundle p mails empty))
(define (drop-player p iw)
  (broadcast-universe (play-remove p iw)))
(define (create-a-body size)
  (define x (+ size (random (- WIDTH size))))
  (define y (+ size (random (- HEIGHT size))))
  (body size (make-rectangular x y)))
(define (get-iws p)
  (map ip-iw (append (play-players p) (play-spectators p))))

;;; 14.7: Main, Take Server

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

(define (handle-goto-message s iw msg)
  (cond [(and (play? s) (goto? msg)) (goto s iw msg)]
        [else                        (empty-bundle s)]))

(define (empty-bundle s)
  (make-bundle s empty empty))

;; The Join State & Network Events

(define ((make-connection adder) u iw)
  (define player (named-player iw))
  (define mails  (list (make-mail iw (ip-id player))))
  (make-bundle (adder u player) mails empty))

(define (join-add-player j new-p)
  (join (cons new-p (join-clients j)) (join-time j)))

(define add-player (make-connection join-add-player))
(define add-spectator (make-connection play-add-spectator))

(define (named-player iw)
  (create-player iw (symbol->string (gensym (iworld-name iw)))))

(define (create-player iw n)
  (ip iw n (create-a-body PLAYER-SIZE) empty))

(define (drop-client j iw)
  (empty-bundle (join-remove j iw)))

(define (join-remove j iw)
  (join (rip iw (join-clients j)) (join-time j)))

(define (rip iw players)
  (remove iw players (lambda (iw p) (iworld=? iw (ip-iw p)))))

;; The Join State and Tick Events

(define (wait-or-play j)
  (cond [(keep-waiting? j) (keep-waiting j)]
        [else              (start-game j)]))

(define (keep-waiting? j)
  (or (> PLAYER-LIMIT (length (join-clients j)))
      (> WAIT-TIME (join-time j))))

(define (keep-waiting j)
  (set-join-time! j (add1 (join-time j)))
  (time-broadcast j))

(define (time-broadcast j)
  (define iworlds (map ip-iw (join-clients j)))
  (define load% (min 1 (/ (join-time j) WAIT-TIME)))
  (make-bundle j (broadcast iworlds load%) empty))

(define (broadcast iws msg)
  (map (lambda (iw) (make-mail iw msg)) iws))

(define (start-game j)
  (define clients (join-clients j))
  (define cupcakes (bake-cupcakes (length clients)))
  (broadcast-universe (play clients cupcakes empty false)))

(define (bake-cupcakes player#)
  (for/list ([i (in-range (* player# FOOD*PLAYERS))])
    (create-a-body CUPCAKE)))

(define (serialize-universe p id)
  (define (sanitize-players pl)
    (define id2 (player-id pl))
    (if (equal? id id2)
        pl
        (player id2 (player-body pl) false)))
  (define (sanitize-ip ip) (sanitize-players (ip-player ip)))
  (define serialized-players (map sanitize-ip (play-players p)))
  (list SERIALIZE serialized-players (play-food p)))

;; The Play State and Network Events

(define (move-and-eat pu)
  (define nplayer (move-player* (play-players pu)))
  (define nfood (feed-em-all pu nplayer (play-food pu)))
  (progress nplayer nfood (play-spectators pu) (play-generate pu)))

(define (move-player* players)
  (for/list ([p players])
    (define waypoints (ip-waypoints p))
    (cond [(empty? waypoints) p]
          [else (define body (ip-body p))
                (define nwpts (move-toward-waypoint body waypoints))
                (ip (ip-iw p) (ip-id p) body nwpts)])))

(define (move-toward-waypoint body waypoints)
  (define goal (first waypoints))
  (define bloc (body-loc body))
  (define line (- goal bloc))
  (define dist (magnitude line)) ;; in pixels / tick
  (define speed (/ BASE-SPEED (body-size body)))
  (cond [(<= dist speed)
         (set-body-loc! body goal)
         (rest waypoints)]
        [else
         (define velocity (/ (* speed line) dist))
         (set-body-loc! body (+ bloc velocity))
         waypoints]))

(define (feed-em-all pu players foods)
  (for/fold ([foods foods]) ([p players])
    (eat-all-the-things pu p foods)))

(define (generate-cupcake? pu)
  (let ((generate (play-generate pu)))
    (set-play-generate! pu (not generate))
    generate))

(define (eat-all-the-things pu player foods)
  (define b (ip-body player))
  (for/fold ([foods empty]) ([f foods])
    (cond [(body-collide? f b)
           (set-body-size! b (+ PLAYER-FATTEN-DELTA (body-size b)))
           (if (generate-cupcake? pu)
               (cons (create-a-body CUPCAKE) foods)
               foods)]
          [else (cons f foods)])))

(define (body-collide? s1 s2)
  (<= (magnitude (- (body-loc s1) (body-loc s2)))
      (+ (body-size s1) (body-size s2))))

(define (progress pls foods spectators gen)
  (define p (play pls foods spectators gen))
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
