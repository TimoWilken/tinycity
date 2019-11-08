#!/usr/bin/racket
#lang racket

(require racket/gui)
(require racket/draw)

(define *tile-size* 128)
(define *show-tile-grid* #t)

(define (draw-base-tile dc type x y)
  (send dc set-pen "brown" 1/64 (if *show-tile-grid* 'solid 'transparent))
  (send dc set-brush (match type
                       ['grass "green"]
                       ['concrete "gray"]
                       ['water "darkblue"]) 'solid)
  (send dc draw-rectangle x y 1 1))

(define (tile-anchor-coords anchor x y)
  (match anchor
    ['north (values (+ x 1/2) y)]
    ['east (values (+ x 1) (+ y 1/2))]
    ['south (values (+ x 1/2) (+ y 1))]
    ['west (values x (+ y 1/2))]
    ['center (values (+ x 1/2) (+ y 1/2))]))

(define (move-coords direction from-x from-y distance)
  (match direction
    ['north (values from-x (- from-y distance))]
    ['east (values (+ from-x distance) from-y)]
    ['south (values from-x (+ from-y distance))]
    ['west (values (- from-x distance) from-y)]))

(define (draw-road dc direction from-tile-x from-tile-y for-tiles)
  (let*-values ([(from-point-x from-point-y) (tile-anchor-coords direction from-tile-x from-tile-y)]
                [(to-point-x to-point-y) (move-coords direction from-point-x from-point-y for-tiles)])
    (send dc set-pen "black" 1/2 'solid)
    (send dc draw-line from-point-x from-point-y to-point-x to-point-y)
    (send dc set-pen "white" 1/16 'solid)
    (send dc draw-line from-point-x from-point-y to-point-x to-point-y)))

(define (main-canvas-paint canvas dc)
  (send dc set-scale *tile-size* *tile-size*)
  (send dc set-smoothing 'aligned)
  (for*/list ([x (range 6)] [y (range 6)])
    (draw-base-tile dc 'grass x y))
  (for*/list ([x (range 3 6)] [y (range 3 5)])
    (draw-base-tile dc 'concrete x y))
  (for*/list ([x (range 6)] [y (range 6 10)])
    (draw-base-tile dc 'water x y))
  (draw-road dc 'south 1 1 3)
  (draw-road dc 'east 2 1 5))

(let ([frame (new frame% [label "TinyCity"] [width 640] [height 480])])
  (new canvas% [parent frame] [paint-callback main-canvas-paint])
  (send frame show #t))
