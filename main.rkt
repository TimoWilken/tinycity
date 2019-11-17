#!/usr/bin/racket
#lang typed/racket
(require typed/racket/gui)
(require typed/racket/draw)

(define-type Terrain (U 'grass 'water 'concrete))
(define-type Tile-Anchor (U 'north 'south 'east 'west 'center))
(define-type Zone-Type (U 'residential 'commercial 'industrial))
(define-type Building (U 'police 'fire))
(define-type Tile
  (Pairof Terrain (U Null
                     (Pairof 'road (Listof Tile-Anchor))
                     (List 'zone Zone-Type)
                     (List 'building Building))))
(define-type World (Mutable-HashTable (List Integer Integer) Tile))

(define *tile-size* : Exact-Nonnegative-Integer 32)
(define *world-size* : Exact-Nonnegative-Integer 100)
(define *scroll-step* : Nonnegative-Real 1/32)

(define *terrain-brushes* : (Immutable-HashTable Terrain (Instance Brush%))
  (let ([terrain-colors : (Immutable-HashTable Terrain String)
                        #hash((grass . "green")
                              (concrete . "gray")
                              (water . "darkblue"))])
    (make-immutable-hash
     (hash-map terrain-colors
               (λ ([terrain : Terrain] [color : String])
                 : (Pairof Terrain (Instance Brush%))
                 (cons terrain (new brush% [color color])))))))

(define *transparent-pen* : (Instance Pen%)
  (send the-pen-list find-or-create-pen "black" 0 'transparent))

(define *tile-grid-pen* : (Instance Pen%)
  (send the-pen-list find-or-create-pen "brown" 1/64 'transparent))

(define *transparent-brush* : (Instance Brush%)
  (new brush% [style 'transparent]))

(define *asphalt-pen* : (Instance Pen%)
  (send the-pen-list find-or-create-pen "black" 1/2 'solid 'butt))

(define *road-markings-pen* : (Instance Pen%)
  (send the-pen-list find-or-create-pen "white" 1/16 'solid 'butt))

(define (pixels->tiles [x : Integer] [y : Integer]) : (values Integer Integer)
  (values (quotient x *tile-size*) (quotient y *tile-size*)))

(: dir-negate (-> Tile-Anchor Tile-Anchor))
(define/match (dir-negate dir)
  [('north) 'south]
  [('south) 'north]
  [('east) 'west]
  [('west) 'east])

(: tile-anchor-coords (-> Integer Integer Tile-Anchor (values Real Real)))
(define/match (tile-anchor-coords x y anchor)
  [(_ _ 'north ) (values (+ x 1/2)    y     )]
  [(_ _ 'east  ) (values (+ x 1  ) (+ y 1/2))]
  [(_ _ 'south ) (values (+ x 1/2) (+ y 1  ))]
  [(_ _ 'west  ) (values    x      (+ y 1/2))]
  [(_ _ 'center) (values (+ x 1/2) (+ y 1/2))])

(define (tile-neighbours [x : Integer] [y : Integer])
  : (Listof (List Tile-Anchor Integer Integer))
  `((west ,(sub1 x) ,y)
    (east ,(add1 x) ,y)
    (north ,x ,(sub1 y))
    (south ,x ,(add1 y))))

(define (tile-has-road? [world : World] [x : Integer] [y : Integer]) : Boolean
  (match (hash-ref world (list x y) #f)
    [(list _ 'road _ ...) #t]
    [_ #f]))

(define (draw-real-point [dc : (Instance DC<%>)] [x : Real] [y : Real]
                         [color : String] [radius : Nonnegative-Real]) : Void
  ;; (send dc draw-point ...) doesn't draw a point/circle, but a short line.
  ;; Because we've scaled the DC by a large number, this line is very visible.
  ;; Hence, we need to draw the "point" using an ellipse. The disadvantage is
  ;; that we can't use the current pen!
  (send dc set-pen *transparent-pen*)
  (send dc set-brush color 'solid)
  ;; TODO: Can we encode the sign of these in the type system without using max?
  (let ([top-left-x : Nonnegative-Real (max 0 (- x radius))]
        [top-left-y : Nonnegative-Real (max 0 (- y radius))]
        [diameter (* 2 radius)])
    (send dc draw-ellipse top-left-x top-left-y diameter diameter)))

(define (draw-road-tile [dc : (Instance DC<%>)] [x : Integer] [y : Integer]
                        [connect-directions : (Listof Tile-Anchor)]) : Void
  (match connect-directions
    ['() (void)]
    [(list 'center)
     (let-values ([(ctr-x ctr-y) (tile-anchor-coords x y 'center)])
       (draw-real-point dc ctr-x ctr-y "black" 1/4)
       (draw-real-point dc ctr-x ctr-y "white" 1/32))]
    [(list 'center other-dirs ...)
     (draw-road-tile dc x y other-dirs)]
    [(list dir other-dirs ...)
     (let-values ([(ctr-x ctr-y) (tile-anchor-coords x y 'center)]
                  [(edge-x edge-y) (tile-anchor-coords x y dir)])
       (draw-real-point dc ctr-x ctr-y "black" 1/4)
       (send dc set-pen *asphalt-pen*)
       (send dc draw-line ctr-x ctr-y edge-x edge-y)
       (draw-road-tile dc x y other-dirs)
       (draw-real-point dc ctr-x ctr-y "white" 1/32)
       (send dc set-pen *road-markings-pen*)
       (send dc draw-line ctr-x ctr-y edge-x edge-y))]))

(define (draw-tile [dc : (Instance DC<%>)] [x : Integer] [y : Integer]
                   [tile-info : Tile]) : Void
  (match-let ([(list base cover-info ...) tile-info])
    (send dc set-pen *tile-grid-pen*)
    (send dc set-brush (hash-ref *terrain-brushes* base))
    (send dc draw-rectangle x y 1 1)
    (match cover-info
      ['() (void)]
      [(list 'zone zone-type)
       (void)] ;; TODO draw zone
      [(list 'road connected-dirs ...)
       (draw-road-tile dc x y connected-dirs)]
      [(list 'building building-type)
       (void)]))) ;; TODO draw building

(define (place-road! [world : World] [x : Integer] [y : Integer]) : Void
  ;; Create and connect a new road tile.
  (create-road! world x y '(center))
  (for ([neighbour : (List Tile-Anchor Integer Integer) (tile-neighbours x y)])
    (match-let ([(list dir nx ny) neighbour])
      ; assume that if a road exists here, we can create more
      (when (tile-has-road? world nx ny)
        (create-road! world nx ny (list (dir-negate dir)))
        (create-road! world x y (list dir))))))

(define (remove-road! [world : World] [x : Integer] [y : Integer]) : Void
  ;; Remove and disconnect an existing road tile.
  (hash-update! world (list x y)
                (λ ([tile : Tile]) : Tile
                   (match tile
                     [(list base 'road _ ...) (list base)]
                     [tile tile])))
  (for ([neighbour : (List Tile-Anchor Integer Integer) (tile-neighbours x y)])
    (match-let ([(list dir nx ny) neighbour])
      (remove-road-direction! world nx ny (dir-negate dir)))))

(define (remove-road-direction! [world : World] [x : Integer] [y : Integer]
                                [direction : Tile-Anchor]) : Void
  (hash-update! world (list x y)
                (λ ([tile : Tile]) : Tile
                   (match tile
                     [(list base 'road connected-dirs ...)
                      (list* base 'road (remove direction connected-dirs))]
                     [tile tile]))))

(define (create-road! [world : World] [x : Integer] [y : Integer]
                      [connect-dirs : (Listof Tile-Anchor)]) : Void
  (unless (can-build-at? world x y 'road)
    (raise-arguments-error 'create-road! "can't build road here" "x" x "y" y))
  (hash-update!
   world (list x y)
   (λ ([tile : Tile]) : Tile
      (match tile
        [(list base 'road connected-dirs ...)
         (list* base 'road (set-union connected-dirs connect-dirs))]
        [(list base _ ...)
         ;; Overwrite with road if one doesn't already exist here. (Just adds a
         ;; road if no tile cover exists.) TODO: is this the right thing to do?
         ;; Does (can-build-at? ...) guarantee that we can overwrite this tile?
         (list* base 'road connect-dirs)]))))

(define (can-build-at? [world : World] [x : Integer] [y : Integer]
                       ;; TODO: narrow down type of `type'
                       [type : Symbol]) : Boolean
  (let ([tile (hash-ref world (list x y) (λ () '()))])
    (match tile
      [(list) #f]
      [(list base) (not (eq? base 'water))]
      [(list _ 'road _ ...) (eq? type 'road)]
      [(list _ 'building _ ...) #f]
      [(list _ 'zone _) #f])))

(define *world* : World
  (let ([world : World (make-hash)])
    (for* ([x : Integer (range *world-size*)]
           [y : Integer (range 15)])
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

(define (draw-cursor-tile [dc : (Instance DC<%>)] [x : Integer] [y : Integer])
  : Void
  (send dc set-brush *transparent-brush*)
  ;; We need to hardcode pen widths in multiple places here as the type system
  ;; doesn't seem to recognise that (- 1 pen-width) will be positive. We can't
  ;; use (Refine ...) as that only works with Integers.
  (let* ([pen-width 1/32] [inner-rect-size 31/32])
    (send dc set-pen (send the-pen-list find-or-create-pen "red" pen-width 'solid))
    (send dc draw-rectangle (+ x (/ pen-width 2)) (+ y (/ pen-width 2))
          inner-rect-size inner-rect-size)))

(define (main-canvas-paint [canvas : (Instance Canvas%)]
                           [dc : (Instance DC<%>)]) : Void
  (send dc set-scale *tile-size* *tile-size*)
  (send dc set-smoothing 'smoothed)
  (let*-values ([(left-px top-px) (send canvas get-view-start)]
                [(width-px height-px) (send canvas get-client-size)]
                [(left top) (pixels->tiles left-px top-px)]
                [(right bottom) (pixels->tiles (+ left-px width-px)
                                               (+ top-px height-px))])
    (for ([(pos tile) *world*])
      (match-let ([(list x y) pos])
        (when (and (<= left x right) (<= top y bottom))
          (draw-tile dc x y tile))))))

(define (draw-world-tile! [dc : (Instance DC<%>)] [world : World] [x : Integer]
                          [y : Integer]) : Void
  (let ([tile (hash-ref *world* (list x y) #f)])
    (when tile (draw-tile dc x y tile))))

(define (draw-tile-with-neighbours! [dc : (Instance DC<%>)] [world : World]
                                    [x : Integer] [y : Integer]) : Void
  (draw-world-tile! dc world x y)
  (for ([tile : (List Tile-Anchor Integer Integer) (tile-neighbours x y)])
    (match-let ([(list _ nx ny) tile])
      (draw-world-tile! dc world nx ny))))

(define (tool-handle-event [tool : (Listof Symbol)]
                           [event : (Instance Mouse-Event%)]
                           [dc : (Instance DC<%>)]) : Void
  (define-values (tx ty)
    (pixels->tiles (send event get-x) (send event get-y)))
  (when (send event button-up? 'left)
    (match tool
      ['(build road)
       (when (with-handlers ([exn:fail:contract? (λ (e) #f)])
               (place-road! *world* tx ty) #t)
         (draw-tile-with-neighbours! dc *world* tx ty))]
      ['(bulldoze)
       (remove-road! *world* tx ty)
       (draw-tile-with-neighbours! dc *world* tx ty)]
      [(list 'build 'zone zone-type)
       (void)]
      [_ (void)])))

(define main-canvas%
  (class canvas%
    (init-field [toplevel : (Instance Frame%)])
    (when (not (send toplevel has-status-line?))
      (send toplevel create-status-line))
    (inherit init-auto-scrollbars scroll get-client-size get-virtual-size
             get-view-start get-dc suspend-flush resume-flush)

    (define previous-cursor-tile : (Option (List Integer Integer)) #f)

    (define/override (on-event event)
      (let-values ([(origin-x origin-y) (get-view-start)])
        (send event set-x (+ (send event get-x) origin-x))
        (send event set-y (+ (send event get-y) origin-y)))
      (define-values (tx ty)
        (pixels->tiles (send event get-x) (send event get-y)))
      (suspend-flush)
      (match previous-cursor-tile
        [#f (void)]
        [(list x y) (draw-world-tile! (get-dc) *world* x y)])
      (let ([sel-tool (get-selected-tool)])
        (when sel-tool (tool-handle-event sel-tool event (get-dc))))
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
    (let ([width+height (* *world-size* *tile-size*)])
      (init-auto-scrollbars width+height width+height 0 0))))

(define (on-select-tool [radio-box : (Instance Radio-Box%)]
                        [event : (Instance Control-Event%)]) : Void
  ;; TODO: decorate the cursor tile depending on the selected tool?
  (send gui:toplevel-frame set-status-text
        (format "~a" (send radio-box get-selection))))

(define *tools* : (Listof (Pairof String (Listof Symbol)))
  '(("&Query" query)
    ("&Bulldoze" bulldoze)
    ("&Road" build road)
    ("Zone: Resi&dential" build zone residential)
    ("Zone: &Commercial" build zone commercial)
    ("Zone: &Industrial" build zone industrial)))

(define (get-selected-tool) : (Option (Listof Symbol))
  (let ([sel-name (send gui:tool-selection get-selection)])
    (if sel-name
        (rest (list-ref *tools* sel-name))
        #f)))

(define gui:toplevel-frame : (Instance Frame%)
  (new frame% [label "TinyCity"] [width 640] [height 480]))
(module+ main
  (send gui:toplevel-frame show #t))

(define gui:main-pane : (Instance Horizontal-Pane%)
  (new horizontal-pane% [parent gui:toplevel-frame]))
(define gui:sidebar-panel : (Instance Vertical-Panel%)
  (new vertical-panel% [parent gui:main-pane] [style '(auto-vscroll)]
       [alignment '(center top)] [stretchable-width #f]))

(define gui:tool-selection : (Instance Radio-Box%)
  (new radio-box% [label "Selected tool"] [parent gui:sidebar-panel]
       [style '(vertical vertical-label)] [callback on-select-tool]
       [choices (map (ann first (-> (Pairof String (Listof Symbol)) String))
                     *tools*)]))

(define gui:main-canvas
  (new main-canvas% [parent gui:main-pane] [toplevel gui:toplevel-frame]))
