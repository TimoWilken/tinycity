#!/usr/bin/racket
#lang racket

(require racket/gui)
(require racket/draw)

(define *tile-size* 32)

(define (color-tile color)
  (let* ([bitmap (make-bitmap *tile-size* *tile-size*)]
         [dc (new bitmap-dc% [bitmap bitmap])])
    (send dc set-smoothing 'aligned)
    (send dc set-pen "black" 0 'transparent)
    (send dc set-brush color 'solid)
    (send dc draw-rectangle 0 0 *tile-size* *tile-size*)
    (values bitmap dc)))

(define (make-monocolor-rect color)
  (let-values ([(bitmap _) (color-tile color)])
    bitmap))

(define grass (make-monocolor-rect "green"))
(define concrete (make-monocolor-rect "gray"))
(define water (make-monocolor-rect "darkblue"))
(define road-ns
  (let-values ([(bitmap dc) (color-tile "green")])
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle (/ *tile-size* 4) 0 (/ *tile-size* 2) *tile-size*)
    (send dc set-pen "white" (/ *tile-size* 16) 'short-dash)
    (send dc draw-line (/ *tile-size* 2) 0 (/ *tile-size* 2) *tile-size*)
    bitmap))

(define (draw-tile dc type x y)
  (send dc draw-bitmap
        (match type
          ['road road-ns]
          ['grass grass]
          ['concrete concrete]
          ['water water])
        (* x *tile-size*)
        (* y *tile-size*)))

(define (draw-road dc direction from-x from-y len)
  (define (step-coords step)
    (let-values ([(tile-x tile-y offset)
                  (apply values (map ((curry *) *tile-size*)
                                     (list from-x from-y step)))])
      (match direction
        ['north (list tile-x (- tile-y offset))]
        ['east (list (+ tile-x offset) tile-y)]
        ['south (list tile-x (+ tile-y offset))]
        ['west (list (- tile-x offset) tile-y)])))

  (for/list ([coords (map step-coords (range len))])
    (send dc draw-bitmap road-ns . coords)))

(define (main-canvas-paint canvas dc)
  (draw-tile dc 'grass 0 0)
  (draw-tile dc 'grass 0 1)
  (draw-tile dc 'grass 1 0)
  (draw-road dc 'south 1 1 5)
  (draw-road dc 'east 2 1 5))

(define (main)
  (define frame
    (new frame% [label "Example"] [width 300] [height 300]))
  (new canvas% [parent frame] [paint-callback main-canvas-paint])
  (send frame show #t))

(main)
