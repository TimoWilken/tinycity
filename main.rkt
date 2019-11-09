#!/usr/bin/racket
#lang racket
(require racket/gui)
(require racket/draw)

(define *tile-size* 128)
(define *show-tile-grid* #t)
(define *terrain-colors*
  #hash((grass . "green") (concrete . "gray") (water . "darkblue")))

(struct tile-pos (x y))
(struct subtile-pos (x y anchor))

(define (draw-base-tile dc type pos)
  (send dc set-pen "brown" 1/64 (if *show-tile-grid* 'solid 'transparent))
  (send dc set-brush (hash-ref *terrain-colors* type) 'solid)
  (send dc draw-rectangle (tile-pos-x pos) (tile-pos-y pos) 1 1))

(define (tile-anchor-coords pos)
  (let ([x (subtile-pos-x pos)]
        [y (subtile-pos-y pos)])
    (match (subtile-pos-anchor pos)
      ['north (values (+ x 1/2) y)]
      ['east (values (+ x 1) (+ y 1/2))]
      ['south (values (+ x 1/2) (+ y 1))]
      ['west (values x (+ y 1/2))]
      ['center (values (+ x 1/2) (+ y 1/2))])))

(define (move-coords pos direction distance)
  (let ([x (tile-pos-x pos)]
        [y (tile-pos-y pos)])
    (match direction
      ['north (values x (- y distance))]
      ['east (values (+ x distance) y)]
      ['south (values x (+ y distance))]
      ['west (values (- x distance) y)])))

(define (draw-real-point dc x y color radius)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush color 'solid)
  (let ([top-left-x (- x radius)]
        [top-left-y (- y radius)]
        [diameter (* 2 radius)])
    (send dc draw-ellipse top-left-x top-left-y diameter diameter)))

(define/match (draw-road-tile dc tile connect-directions)
  [(_ _ (list)) (void)]
  [(dc (tile-pos x y) connect-directions)
   (define (draw-center-point)
     (let-values ([(x y) (tile-anchor-coords (subtile-pos x y 'center))])
       (draw-real-point dc x y "black" 1/4)
       (draw-real-point dc x y "white" 1/32)))

   (match connect-directions
     [(list-no-order 'center other-dir)
      (begin (draw-center-point)
             (send dc set-pen (send the-pen-list find-or-create-pen "black" 1/2 'solid 'butt))
             (let-values ([(x1 y1) (tile-anchor-coords (subtile-pos x y 'center))]
                         [(x2 y2) (tile-anchor-coords (subtile-pos x y other-dir))])
               (send dc draw-line x1 y1 x2 y2)
               (send dc set-pen (send the-pen-list find-or-create-pen "white" 1/16 'solid 'butt))
               (send dc draw-line x1 y1 x2 y2)))]
     [(list-no-order 'center other-dirs ...) (draw-road-tile dc tile other-dirs)]
     [(list-no-order 'north 'south 'east 'west)
      (begin empty)]
     [(list-no-order 'east 'west)
      (begin empty)]
     [(list-no-order 'north 'south)
      (begin empty)])])

(define (draw-road dc direction from-tile-x from-tile-y for-tiles)
  (let*-values ([(from-point-x from-point-y) (tile-anchor-coords (subtile-pos from-tile-x from-tile-y direction))]
                [(to-point-x to-point-y) (move-coords (tile-pos from-point-x from-point-y) direction for-tiles)])
    (send dc set-pen "black" 1/2 'solid)
    (send dc draw-line from-point-x from-point-y to-point-x to-point-y)
    (send dc set-pen "white" 1/16 'solid)
    (send dc draw-line from-point-x from-point-y to-point-x to-point-y)))

(define (main-canvas-paint canvas dc)
  (send dc set-scale *tile-size* *tile-size*)
  (send dc set-smoothing 'aligned)
  (for*/list ([x (range 8)] [y (range 6)])
    (draw-base-tile dc 'grass (tile-pos x y)))
  (for*/list ([x (range 3 6)] [y (range 3 5)])
    (draw-base-tile dc 'concrete (tile-pos x y)))
  (for*/list ([x (range 8)] [y (range 6 10)])
    (draw-base-tile dc 'water (tile-pos x y)))
  (draw-road dc 'south 1 1 3)
  (draw-road dc 'east 2 1 5)
  (draw-road-tile dc (tile-pos 3 3) '(center east))
  (draw-road-tile dc (tile-pos 4 3) '(west east))
  (draw-road-tile dc (tile-pos 4 4) '(north south west east)))

(let* ([frame (new frame% [label "TinyCity"] [width 640] [height 480])]
       [canvas (new canvas% [parent frame]
                    ;; This uses a lambda so that main-canvas-paint can be
                    ;; updated while the program is running!
                    [paint-callback (λ args (apply main-canvas-paint args))]
                    [style '(hscroll vscroll)])])
  (send canvas init-auto-scrollbars (* 25 *tile-size*) (* 25 *tile-size*) 0 0)
  (send frame show #t))
