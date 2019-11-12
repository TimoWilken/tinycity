#!/usr/bin/racket
#lang racket
(require profile)
(require racket/gui)
(require racket/draw)

(define *tile-size* 32)
(define *world-size* 100)
(define *scroll-step* 1/32)
(define *terrain-brushes*
  (make-immutable-hash
   (hash-map #hash((grass . "green")
                   (concrete . "gray")
                   (water . "darkblue"))
             (λ (terrain color)
               (cons terrain (send the-brush-list
                                   find-or-create-brush color 'solid))))))
(define *transparent-pen*
  (send the-pen-list find-or-create-pen "black" 0 'transparent))
(define *tile-grid-pen*
  (send the-pen-list find-or-create-pen "brown" 1/64 'transparent))
(define *cursor-tile-brush*
  (send the-brush-list find-or-create-brush "black" 'transparent))
(define *cursor-tile-pen*
  (send the-pen-list find-or-create-pen "red" 1/32 'solid))
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

(define (tile-neighbours x y)
  `((west ,(sub1 x) ,y)
    (east ,(add1 x) ,y)
    (north ,x ,(sub1 y))
    (south ,x ,(add1 y))))

(define (tile-has-road? world x y)
  (match (hash-ref world (list x y) #f)
    [(list _ 'road _ ...) #t]
    [_ #f]))

(define (draw-real-point dc x y color radius)
  ;; (send dc draw-point ...) doesn't draw a point/circle, but a short line.
  ;; Because we've scaled the DC by a large number, this line is very visible.
  ;; Hence, we need to draw the "point" using an ellipse. The disadvantage is
  ;; that we can't use the current pen!
  (send dc set-pen *transparent-pen*)
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
    (send dc set-pen *tile-grid-pen*)
    (send dc set-brush (hash-ref *terrain-brushes* base))
    (send dc draw-rectangle x y 1 1)
    (let draw-tile-cover ([cover-info cover-info])
      (match cover-info
        ['() (void)]
        [(list 'zone zone-type)
         (void)] ;; TODO draw zone
        [(list 'road connected-dirs ...)
         (draw-road-tile dc x y connected-dirs)]
        [(list 'building building-type)
         (void)])))) ;; TODO draw building

(define (place-road! world x y)
  ;; Create and connect a new road tile.
  (create-road! world x y '(center))
  (for ([neighbour (tile-neighbours x y)])
    (match-let ([(list dir nx ny) neighbour])
      ; assume that if a road exists here, we can create more
      (when (tile-has-road? world nx ny)
        (create-road! world nx ny (list (dir-negate dir)))
        (create-road! world x y (list dir))))))

(define (remove-road! world x y)
  ;; Remove and disconnect an existing road tile.
  (hash-update! world (list x y)
                (match-lambda
                  [(list base 'road _ ...) (list base)]
                  [tile tile]))
  (for ([neighbour (tile-neighbours x y)])
    (match-let ([(list dir nx ny) neighbour])
      (remove-road-direction! world nx ny (dir-negate dir)))))

(define (remove-road-direction! world x y direction)
  (hash-update! world (list x y)
                (match-lambda
                  [(list base 'road connected-dirs ...)
                   (list* base 'road (remove direction connected-dirs))]
                  [tile tile])))

(define (create-road! world x y connect-dirs)
  (unless (can-build-at? world x y 'road)
    (raise-arguments-error 'create-road! "can't build road here" "x" x "y" y))
  (hash-update! world (list x y)
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
      [(list _ 'road _ ...) (eq? type 'road)]
      [(list _ 'building _ ...) #f]
      [(list _ 'zone _) #f])))

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
  (send dc set-brush *cursor-tile-brush*)
  (send dc set-pen *cursor-tile-pen*)
  (let ([pen-width (send *cursor-tile-pen* get-width)])
    (send dc draw-rectangle
          (+ x (/ pen-width 2)) (+ y (/ pen-width 2))
          (- 1 pen-width) (- 1 pen-width))))

(define (main-canvas-paint canvas dc)
  (profile #:delay 0 #:repeat 500 #:order 'self
   (begin
     (send dc set-scale *tile-size* *tile-size*)
     (send dc set-smoothing 'smoothed)
     (let*-values ([(left-px top-px) (send canvas get-view-start)]
                   [(width-px height-px) (send canvas get-client-size)]
                   [(left top) (pixels->tiles left-px top-px)]
                   [(right bottom) (pixels->tiles (+ left-px width-px) (+ top-px height-px))])
       (for ([(pos tile) *world*])
         (match-let ([(list x y) pos])
           (when (and (<= left x right) (<= top y bottom))
             (draw-tile dc x y tile))))))))

(define (draw-world-tile dc world x y)
  (let ([tile (hash-ref *world* (list x y) #f)])
    (when tile (draw-tile dc x y tile))))

(define (draw-tile-with-neighbours dc world x y)
  (draw-world-tile dc world x y)
  (for ([tile (tile-neighbours x y)])
    (match-let ([(list _ nx ny) tile])
      (draw-world-tile dc world nx ny))))

(define (tool-handle-event tool event dc)
  (define-values (tx ty)
    (pixels->tiles (send event get-x) (send event get-y)))
  (when (send event button-up? 'left)
    (match tool
      ['(build road)
       (when (with-handlers ([exn:fail:contract? (λ (e) #f)])
               (place-road! *world* tx ty) #t)
         (draw-tile-with-neighbours dc *world* tx ty))]
      ['(bulldoze)
       (remove-road! *world* tx ty)
       (draw-tile-with-neighbours dc *world* tx ty)]
      [(list 'build 'zone zone-type)
       (void)]
      [_ (void)])))

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
      (define-values (tx ty)
        (pixels->tiles (send event get-x) (send event get-y)))
      (suspend-flush)
      (match previous-cursor-tile
        [#f (void)]
        [(list x y) (draw-world-tile (get-dc) *world* x y)])
      (tool-handle-event (get-selected-tool) event (get-dc))
      (draw-cursor-tile (get-dc) tx ty)
      (resume-flush)
      (set! previous-cursor-tile (list tx ty)))

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
  ;; TODO: change cursor tile decoration?
  (send gui:toplevel-frame set-status-text
        (format "~a" (send radio-box get-selection))))

(define *tools*
  '(("&Query" query)
    ("&Bulldoze" bulldoze)
    ("&Road" build road)
    ("Zone: Resi&dential" build zone residential)
    ("Zone: &Commercial" build zone commercial)
    ("Zone: &Industrial" build zone industrial)))

(define (get-selected-tool)
  (rest (list-ref *tools* (send gui:tool-selection get-selection))))

(define gui:toplevel-frame (new frame% [label "TinyCity"] [width 640] [height 480]))
(define gui:main-pane (new horizontal-pane% [parent gui:toplevel-frame]))
(define gui:sidebar-panel
  (new vertical-panel% [parent gui:main-pane] [style '(auto-vscroll)]
       [alignment '(center top)] [stretchable-width #f]))

(define gui:tool-selection
  (new radio-box% [label "Selected tool"] [parent gui:sidebar-panel]
       [style '(vertical vertical-label)] [callback on-select-tool]
       [choices (map first *tools*)]))

(define gui:main-canvas
  (new main-canvas% [parent gui:main-pane] [toplevel gui:toplevel-frame]))
(send gui:toplevel-frame show #t)
