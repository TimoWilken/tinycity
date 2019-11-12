#!/usr/bin/racket
#lang racket
(require racket/gui)
(require racket/draw)

(define *tile-size* 32)
(define *world-size* 100)
(define *show-tile-grid* #f)
(define *scroll-step* 1/32)
(define *terrain-colors*
  #hash((grass . "green") (concrete . "gray") (water . "darkblue")))
(define *asphalt-pen*
  (send the-pen-list find-or-create-pen "black" 1/2 'solid 'butt))
(define *road-markings-pen*
  (send the-pen-list find-or-create-pen "white" 1/16 'solid 'butt))

(define (member? item lst (cmp eq?))
  (ormap (curry cmp item) lst))

(define (pixels->tiles x y)
  (values (quotient x *tile-size*)
          (quotient y *tile-size*)))

(define/match (dir-negate dir)
  [('north) 'south]
  [('south) 'north]
  [('east) 'west]
  [('west) 'east])

(define/match (tile-anchor-coords x y anchor)
  [(_ _ 'north ) (values (+ x 1/2)    y     )]
  [(_ _ 'east  ) (values (+ x 1  ) (+ y 1/2))]
  [(_ _ 'south ) (values (+ x 1/2) (+ y 1  ))]
  [(_ _ 'west  ) (values    x      (+ y 1/2))]
  [(_ _ 'center) (values (+ x 1/2) (+ y 1/2))])

(define (draw-base-tile dc x y type)
  (send dc set-pen "brown" 1/64 (if *show-tile-grid* 'solid 'transparent))
  (send dc set-brush (hash-ref *terrain-colors* type) 'solid)
  (send dc draw-rectangle x y 1 1))

;; (send dc draw-point ...) doesn't draw a point/circle, but a short line.
;; Because we've scaled the DC by a large number, this line is very visible.
;; Hence, we need to draw the "point" using an ellipse. The disadvantage is that
;; we can't use the current pen!
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
    [(list-no-order 'center)
     (let-values ([(ctr-x ctr-y) (tile-anchor-coords x y 'center)])
       (draw-real-point dc ctr-x ctr-y "black" 1/4)
       (draw-real-point dc ctr-x ctr-y "white" 1/32))]
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

(define (draw-tile dc x y tile-info)
  (match-let ([(list base cover-info ...) tile-info])
    (draw-base-tile dc x y base)
    (let draw-tile-cover ([cover-info cover-info])
      (match cover-info
        ['() (void)]
        [(list 'power other ...)
         (draw-tile-cover other)
         (void)] ;; TODO draw power lines
        [(list 'zone zone-type)
         (void)] ;; TODO draw zone
        [(list 'road connected-dirs ...)
         (draw-road-tile dc x y connected-dirs)]
        [(list 'building building-type)
         (void)])))) ;; TODO draw building

(define (tile-neighbours x y)
  `((west ,(sub1 x) ,y) (east ,(add1 x) ,y) (north ,x ,(sub1 y)) (south ,x ,(add1 y))))

(define (tile-has-road? world x y)
  (match (hash-ref world (list x y) #f)
    [(list _ 'road _ ...) #t]
    [(list _ 'power 'road _ ...) #t]
    [_ #f]))

(define (place-road! world x y)
  (create-road! world x y '(center))
  (connect-road-tile! world x y))

(define (connect-road-tile! world x y)
  (for ([neighbour (tile-neighbours x y)])
    (match-let ([(list dir nx ny) neighbour])
      ; assume that if a road exists here, we can create more
      (when (tile-has-road? world nx ny)
        (create-road! world nx ny (list (dir-negate dir)))
        (create-road! world x y (list dir))))))

(define (create-road! world x y connect-dirs)
  (unless (can-build-at? world x y 'road)
    (raise-arguments-error 'create-road! "can't build road here" "x" x "y" y))
  (hash-update!
   world (list x y)
   (match-lambda
     [(list base 'road connected-dirs ...)
      (list* base 'road (set-union connected-dirs connect-dirs))]
     [(list base _ ...)
      ;; overwrite with or add road -- TODO is this the right thing to do?
      (list* base 'road connect-dirs)])))

(define (can-build-at? world x y type)
  (let can-build-on-tile? ([tile (hash-ref world (list x y) '())])
    (match tile
      [(list) #f]
      [(list base) (not (eq? base 'water))]
      [(list _ 'road _ ...) (member? type '(road power))]
      [(list _ 'building _ ...) #f]
      [(list _ 'zone _) #f]
      [(list base 'power other ...) (can-build-on-tile? (cons base other))])))

(define *world*
  (let ([world (make-hash)])
    (for* ([x (range *world-size*)] [y (range 15)])
      (hash-set! world (list x y)
                 (if (and (<= 3 x 6) (<= 3 y 5)) '(concrete) '(grass))))
    (for* ([x (range *world-size*)] [y (range 15 *world-size*)])
      (hash-set! world (list x y) '(water)))
    (for ([x (range 5 15)])
      (place-road! world x 4))
    (for ([y (range 3 7)])
      (place-road! world 12 y))
    (for ([x '(3 4 4 4 7)] [y '(3 3 4 5 7)])
      (place-road! world x y))
    world))

(define (draw-cursor-tile dc x y)
  (define pen-width 1/32)
  (send dc set-brush "black" 'transparent)
  (send dc set-pen "red" pen-width 'solid)
  (send dc draw-rectangle
        (+ x (/ pen-width 2)) (+ y (/ pen-width 2))
        (- 1 pen-width) (- 1 pen-width)))

(define (main-canvas-paint canvas dc)
  (send dc set-scale *tile-size* *tile-size*)
  (send dc set-smoothing 'smoothed)
  (let*-values ([(left-px top-px) (send canvas get-view-start)]
                [(width-px height-px) (send canvas get-client-size)]
                [(left top) (pixels->tiles left-px top-px)]
                [(right bottom) (pixels->tiles (+ left-px width-px) (+ top-px height-px))])
    (for ([(pos tile) *world*])
      (match-let ([(list x y) pos])
        (when (and (<= left x right) (<= top y bottom))
          (draw-tile dc x y tile))))))

(define (draw-world-tile dc world x y)
  (let ([tile (hash-ref *world* (list x y) #f)])
    (when tile (draw-tile dc x y tile))))

(define main-canvas%
  (class canvas%
    (init-field toplevel)
    (when (not (send toplevel has-status-line?))
      (send toplevel create-status-line))
    (inherit init-auto-scrollbars scroll get-client-size get-virtual-size
             get-view-start get-dc suspend-flush resume-flush)

    (define previous-cursor-tile #f)

    (define/override (on-event event)
      (let-values ([(origin-x origin-y) (get-view-start)])
        (send event set-x (+ (send event get-x) origin-x))
        (send event set-y (+ (send event get-y) origin-y)))
      (send toplevel set-status-text (format "~a" (send event get-event-type)))
      (suspend-flush)
      (match previous-cursor-tile
        [#f (void)]
        [(list x y) (draw-world-tile (get-dc) *world* x y)])

      (let-values ([(tx ty) (pixels->tiles (send event get-x) (send event get-y))])
        (when (send event button-up? 'left)
          (when (with-handlers ([exn:fail:contract? (λ (e) #f)])
                  (place-road! *world* tx ty) #t)
            (draw-world-tile (get-dc) *world* tx ty)
            (for ([tile (tile-neighbours tx ty)])
              (match-let ([(list _ nx ny) tile])
                (draw-world-tile (get-dc) *world* nx ny)))))

        (draw-cursor-tile (get-dc) tx ty)
        (set! previous-cursor-tile (list tx ty)))
      (resume-flush))

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

    (super-new [style '(hscroll vscroll)] [paint-callback main-canvas-paint])
    (init-auto-scrollbars (* *world-size* *tile-size*) (* *world-size* *tile-size*) 0 0)))

(define (on-select-tool radio-box event)
  (send gui:toplevel-frame set-status-text
        (format "~a" (send radio-box get-selection))))

(define gui:toplevel-frame (new frame% [label "TinyCity"] [width 640] [height 480]))
(define gui:main-pane (new horizontal-pane% [parent gui:toplevel-frame]))
(define gui:sidebar-panel
  (new vertical-panel% [parent gui:main-pane] [style '(auto-vscroll)]
       [alignment '(center top)] [stretchable-width #f]))

(define gui:tool-selection
  (new radio-box% [label "Selected tool"] [parent gui:sidebar-panel]
       [style '(vertical vertical-label)] [callback on-select-tool]
       [choices '("&Query" "&Bulldoze" "&Road")]))

(define gui:main-canvas
  (new main-canvas% [parent gui:main-pane] [toplevel gui:toplevel-frame]))
(send gui:toplevel-frame show #t)
