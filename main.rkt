#!/usr/bin/racket
#lang racket

(require racket/gui)
(require racket/draw)

(define *tile-size* 32)

(define (blank-tile)
  (let ([bitmap (make-bitmap *tile-size* *tile-size*)])
    (values bitmap (new bitmap-dc% [bitmap bitmap]))))

(define (make-monocolor-rect color)
  (let-values ([(bitmap dc) (blank-tile)])
    (send dc set-brush color 'solid)
    (send dc draw-rectangle 0 0 *tile-size* *tile-size*)
    bitmap))

(define road
  (let-values ([(bitmap dc) (blank-tile)])
    (send dc set-smoothing 'aligned)
    (send dc set-brush "green" 'solid)
    (send dc draw-rectangle 0 0 *tile-size* *tile-size*)
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle (/ *tile-size* 4) 0 (/ *tile-size* 2) *tile-size*)
    (send dc set-pen "white" (/ *tile-size* 16) 'short-dash)
    (send dc draw-line (/ *tile-size* 2) 0 (/ *tile-size* 2) *tile-size*)
    bitmap))

(define grass (make-monocolor-rect "green"))
(define concrete (make-monocolor-rect "gray"))
(define water (make-monocolor-rect "darkblue"))

(define (main-canvas-paint canvas dc)
  (send dc set-scale 3 3)
  (send dc set-text-foreground "blue")
  (send dc draw-text "Don't Panic!" 0 0))

(define (main)
  (define frame
    (new frame% [label "Example"] [width 300] [height 300]))
  (new canvas% [parent frame] [paint-callback main-canvas-paint])
  (send frame show #t))
