#!/usr/bin/racket
#lang racket
(require racket/gui)
(require racket/draw)

(define *tile-size* 64)
(define *show-tile-grid* #t)
(define *terrain-colors*
  #hash((grass . "green") (concrete . "gray") (water . "darkblue")))
(define *asphalt-pen*
  (send the-pen-list find-or-create-pen "black" 1/2 'solid 'butt))
(define *road-markings-pen*
  (send the-pen-list find-or-create-pen "white" 1/16 'solid 'butt))

(define (draw-base-tile dc x y type)
  (send dc set-pen "brown" 1/64 (if *show-tile-grid* 'solid 'transparent))
  (send dc set-brush (hash-ref *terrain-colors* type) 'solid)
  (send dc draw-rectangle x y 1 1))

(define/match (tile-anchor-coords x y anchor)
  [(_ _ 'north ) (values (+ x 1/2)    y     )]
  [(_ _ 'east  ) (values (+ x 1  ) (+ y 1/2))]
  [(_ _ 'south ) (values (+ x 1/2) (+ y 1  ))]
  [(_ _ 'west  ) (values    x      (+ y 1/2))]
  [(_ _ 'center) (values (+ x 1/2) (+ y 1/2))])

(define/match (move-coords x y direction distance)
  [(_ _ 'north _) (values x (- y distance))]
  [(_ _ 'east  _) (values (+ x distance) y)]
  [(_ _ 'south _) (values x (+ y distance))]
  [(_ _ 'west  _) (values (- x distance) y)])

(define (draw-real-point dc x y color radius)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush color 'solid)
  (let ([top-left-x (- x radius)]
        [top-left-y (- y radius)]
        [diameter (* 2 radius)])
    (send dc draw-ellipse top-left-x top-left-y diameter diameter)))

(define (draw-road-tile dc x y connect-directions)
  (match connect-directions
    ['() (void)]
    [(list-no-order 'center other-dirs ...)
     (draw-road-tile dc x y other-dirs)]
    [(list-no-order dir other-dirs ...)
     (let-values ([(ctr-x ctr-y) (tile-anchor-coords x y 'center)]
                  [(edge-x edge-y) (tile-anchor-coords x y dir)])
       (draw-real-point dc ctr-x ctr-y "black" 1/4)
       (send dc set-pen *asphalt-pen*)
       (send dc draw-line ctr-x ctr-y edge-x edge-y)
       (draw-road-tile dc x y other-dirs)
       (draw-real-point dc ctr-x ctr-y "white" 1/32)
       (send dc set-pen *road-markings-pen*)
       (send dc draw-line ctr-x ctr-y edge-x edge-y))]))

(define (draw-world dc world)
  (for ([(pos tile) (in-hash world)])
    (match-let ([(list x y) pos]
                [(list base cover-info ...) tile])
      (draw-base-tile dc x y base)
      (draw-tile-cover dc x y cover-info))))

(define (draw-tile-cover dc x y cover-info)
  (match cover-info
    ['() (void)]
    [(list 'power other ...)
     (draw-tile-cover dc x y other)
     (void)] ;; TODO draw power lines
    [(list 'zone zone-type)
     (void)] ;; TODO draw zone
    [(list 'road connected-dirs ...)
     (draw-road-tile dc x y connected-dirs)]
    [(list 'building building-type)
     (void)])) ;; TODO draw building

(define (create-road! world x y connect-dirs)
  (unless (can-build-at? world x y 'road)
    (error 'create-road! "can't build road here"))
  (hash-update!
   world (list x y)
   (match-lambda
     [(list base 'road connected-dirs ...)
      `(,base road . ,(set-union connected-dirs connect-dirs))]
     [(list base _ ...)
      ;; overwrite with or add road -- TODO is this the right thing to do?
      `(,base road . ,connect-dirs)])))

(define (can-build-on-tile? tile new-type)
  (match tile
    ['() #f]
    [(list base) (not (eq? base 'water))]
    [(list _ 'road _ ...)
     (match new-type [(or 'road 'power) #t] [_ #f])]
    [(list _ 'building _ ...) #f]
    [(list _ 'zone _) #f]
    [(list base 'power other ...)
     (can-build-on-tile? (cons base other) new-type)]))

(define (can-build-at? world x y type)
  (can-build-on-tile? (hash-ref world (list x y) '()) type))

(define *world*
  (let ([world (make-hash)])
    (for* ([x (range 10)] [y (range 6)])
      (hash-set! world (list x y)
                 (list (if (and (<= 3 x 6) (<= 3 y 5)) 'concrete 'grass))))
    (for* ([x (range 10)] [y (range 6 10)])
      (hash-set! world (list x y) (list 'water)))
    (for ([x (range 5 9)])
      (create-road! world x 2 '(east west)))
    (create-road! world 3 3 '(center east))
    (create-road! world 4 3 '(west south))
    (create-road! world 4 4 '(north east south))
    world))

(define (main-canvas-paint canvas dc)
  (send dc set-scale *tile-size* *tile-size*)
  (send dc set-smoothing 'aligned)
  (draw-world dc *world*))

(define *scroll-step* 1/5)
(define main-canvas%
  (class canvas%
    (init-field toplevel)
    (when (not (send toplevel has-status-line?))
      (send toplevel create-status-line))
    (inherit scroll get-client-size get-virtual-size get-view-start)

    (define/override (on-event event)
      (let-values ([(origin-x origin-y) (get-view-start)])
        (let ([x (+ (send event get-x) origin-x)]
              [y (+ (send event get-y) origin-y)])
          (send toplevel set-status-text
                (format "(~a, ~a) px; tile (~a, ~a); ~a" x y
                        (quotient x *tile-size*) (quotient y *tile-size*)
                        (send event get-event-type))))))

    (define/override (on-char event)
      (let-values ([(scroll-x scroll-y) (get-view-start)]
                   [(virtual-size-x virtual-size-y) (get-virtual-size)]
                   [(client-size-x client-size-y) (get-client-size)])
        (let ([x (/ scroll-x (- virtual-size-x client-size-x))]
              [y (/ scroll-y (- virtual-size-y client-size-y))])
          (match (send event get-key-code)
            ['wheel-up    (scroll #f (max 0 (- y *scroll-step*)))]
            ['wheel-down  (scroll #f (min 1 (+ y *scroll-step*)))]
            ['wheel-left  (scroll (max 0 (- x *scroll-step*)) #f)]
            ['wheel-right (scroll (min 1 (+ x *scroll-step*)) #f)]
            [_ (void)]))))

    (super-new [style '(hscroll vscroll)] [paint-callback main-canvas-paint])))

(define frame (new frame% [label "TinyCity"] [width 640] [height 480]))
(define canvas (new main-canvas% [parent frame] [toplevel frame]))
(send canvas init-auto-scrollbars (* 25 *tile-size*) (* 25 *tile-size*) 0 0)
(send frame show #t)
