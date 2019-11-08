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
      ['north (tile-pos (+ x 1/2) y)]
      ['east (tile-pos (+ x 1) (+ y 1/2))]
      ['south (tile-pos (+ x 1/2) (+ y 1))]
      ['west (tile-pos x (+ y 1/2))]
      ['center (tile-pos (+ x 1/2) (+ y 1/2))])))

(define (move-coords pos direction distance)
  (let ([x (tile-pos-x pos)]
        [y (tile-pos-y pos)])
    (match direction
      ['north (tile-pos x (- y distance))]
      ['east (tile-pos (+ x distance) y)]
      ['south (tile-pos x (+ y distance))]
      ['west (tile-pos (- x distance) y)])))

(define (draw-road dc direction from-tile-x from-tile-y for-tiles)
  (match-let* ([(tile-pos from-point-x from-point-y) (tile-anchor-coords (subtile-pos from-tile-x from-tile-y direction))]
               [(tile-pos to-point-x to-point-y) (move-coords (tile-pos from-point-x from-point-y) direction for-tiles)])
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
  (draw-road dc 'east 2 1 5))

(let ([frame (new frame% [label "TinyCity"] [width 640] [height 480])])
  (new canvas% [parent frame] [paint-callback main-canvas-paint])
  (send frame show #t))
