#lang racket

(require racket/tcp)
(require json)
(require racket/pretty)

; turret aim
; fire

(define world-size '(64 64))

(struct player (name in out position velocity direction))

; Actions
(define (force x amount)
  (printf "Robot ~A moves ~A units.\n" (player-name x) amount)
  (struct-copy player x [velocity amount]))

(define (torque x dir)
  (printf "Robot ~A turns to ~A.\n" (player-name x) dir)
  (struct-copy player x [direction dir]))

(define (ping p y)
  (printf "Robot ~A sends a ping.\n" (player-name p))
  (let* ([matches (filter (lambda (x)
            (if (and (string=? (player-name (car x)) (player-name p))
                     (> 10 (abs (sqrt
                            (+
                              (expt (- (car (player-position (car x))) (car (player-position p))) 2)
                              (expt (- (cdr (player-position (car x))) (cdr (player-position p))) 2))))))
                #f
                #t))
          y)]
         [match-hash (hash 'pong (map (lambda (x) (hash 
                            'name (player-name (car x)) 
                            'x (car (player-position (car x)))
                            'y (cdr (player-position (car x))))) matches))])
    (printf "Robot ~A got ~A back!\n" (player-name p) (hash-ref match-hash 'pong))
    (displayln (jsexpr->string match-hash) (player-out p))
    (flush-output (player-out p))
    
  p))

(define (turret-aim p dir)
  p)

(define (fire p)
  p)

(define (within-world value)
  (cond
    [(> value 64) 64]
    [(< value 0) 0]
    [else value]))

(define (animate x)
  (let ([new-x (within-world (+ (* (cos (player-direction x)) (player-velocity x)) (car (player-position x))))]
        [new-y (within-world (+ (* (sin (player-direction x)) (player-velocity x)) (cdr (player-position x))))])
    (struct-copy player x [position (cons new-x new-y)])))

(define (collisions players)
  players)

(define (do-action player-action actions)
  (let ([action (hash-ref (cdr player-action) 'action)])
    (cond
      [(hash-has-key? action 'force) (force (car player-action) (hash-ref action 'force))]
      [(hash-has-key? action 'torque) (torque (car player-action) (hash-ref action 'torque))]
      [(hash-has-key? action 'ping) (ping (car player-action) actions)]
      [else (displayln "Unknown action")
            (car player-action)])))

(define (update-world actions)
  (let* ([new-players (map (lambda (x) (do-action x actions)) actions)]
         [animated-players (map animate new-players)]
         [done-players (map collisions animated-players)])
    done-players))

(define (tick p)
  (jsexpr->string
   (hash 'message "new-round"
         'robot (hash 'x (car (player-position p)) 
                      'y (cdr (player-position p)) 
                      'velocity (player-velocity p) 
                      'direction (player-direction p)))))

(define (identify)
  (jsexpr->string
   (hash 'message "identify")))

(define (send-ticks player)
  (write (tick player) (player-out player))
  (flush-output (player-out player)))

(define (get-response player)
  (cons player (string->jsexpr
                (read-line (player-in player)))))

(define (play-round players)
  (if (not (null? players))
      (begin
        (map send-ticks players)
        (update-world (map get-response players)))
      players))

(define (identify-player listener)
  (let-values ([(in out) (tcp-accept listener)])
    (write (identify) out)
    (flush-output out)
    (let ([name (hash-ref (string->jsexpr (read-line in)) 'name)])
      (printf "Robot ~A joins the fight!\n" name)
      (player name in out (cons 0 0) 0 pi))))

(define (append-new-player new-player players) 
  (if (null? players)
      (list new-player)
      (append players (list new-player))))

(define (server listener clients)
  (sleep 1)
  (let ([new-players (play-round clients)])
    (if (tcp-accept-ready? listener)
        (server listener (append-new-player (identify-player listener) new-players))
        (server listener new-players))))

(define (serve port-number)
  (let ([listener (tcp-listen port-number)])
    (server listener '())))

(serve 8000)
