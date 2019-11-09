#!/usr/bin/racket
#lang racket
(require racket/gui)
(require racket/draw)

(define *tile-size* 64)
(define *show-tile-grid* #t)
(define *terrain-colors*
  #hash((grass . "green") (concrete . "gray") (water . "darkblue")))

(define (draw-base-tile dc type x y)
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

(define (make-road-tile-bitmap connect-directions)
  (define/match (angle-from-north direction)
    [('north) 0]
    [('east ) (* 1/2 pi)]
    [('south) pi]
    [('west ) (* 3/2 pi)])

  (let* ([bitmap (make-bitmap *tile-size* *tile-size*)]
         [dc (send bitmap make-dc)])
    (send dc set-scale *tile-size* *tile-size*)
    (send dc set-smoothing 'aligned)
    (match connect-directions
      ['() bitmap]

      [(list-no-order 'center other-dir)
       (draw-real-point dc 1/2 1/2 "black" 1/4)
       (draw-real-point dc 1/2 1/2 "white" 1/32)
       (send dc set-rotation (angle-from-north other-dir))
       (send dc set-pen (send the-pen-list find-or-create-pen "black" 1/2 'solid 'butt))
       (send dc draw-line 1/2 1/2 1/2 0)
       (send dc set-pen (send the-pen-list find-or-create-pen "white" 1/16 'solid 'butt))
       (send dc draw-line 1/2 1/2 1/2 0)
       bitmap]

      [(list-no-order 'center other-dirs ...)
       (make-road-tile-bitmap other-dirs)]

      [(list-no-order 'north 'south 'east 'west)
       bitmap])))

(define (draw-road-tile dc x y connect-directions)
  (send dc set-scale 1 1)
  (send dc draw-bitmap (make-road-tile-bitmap connect-directions) (* x *tile-size*) (* y *tile-size*))
  (send dc set-scale *tile-size* *tile-size*))

(define (draw-road dc direction from-tile-x from-tile-y for-tiles)
  (let*-values ([(from-point-x from-point-y) (tile-anchor-coords from-tile-x from-tile-y direction)]
                [(to-point-x to-point-y) (move-coords from-point-x from-point-y direction for-tiles)])
    (send dc set-pen "black" 1/2 'solid)
    (send dc draw-line from-point-x from-point-y to-point-x to-point-y)
    (send dc set-pen "white" 1/16 'solid)
    (send dc draw-line from-point-x from-point-y to-point-x to-point-y)))

(define (main-canvas-paint canvas dc)
  (send dc set-scale *tile-size* *tile-size*)
  (send dc set-smoothing 'aligned)
  (for*/list ([x (range 8)] [y (range 6)])
    (draw-base-tile dc 'grass x y))
  (for*/list ([x (range 3 6)] [y (range 3 5)])
    (draw-base-tile dc 'concrete x y))
  (for*/list ([x (range 8)] [y (range 6 10)])
    (draw-base-tile dc 'water x y))
  (draw-road dc 'south 1 1 3)
  (draw-road dc 'east 2 1 5)
  (draw-road-tile dc 3 3 '(center east))
  ;(draw-road-tile dc 4 3 '(west east))
  (draw-road-tile dc 4 4 '(north south west east)))

(let* ([frame (new frame% [label "TinyCity"] [width 640] [height 480])]
       [canvas (new canvas% [parent frame]
                    ;; This uses a lambda so that main-canvas-paint can be
                    ;; updated while the program is running!
                    [paint-callback (λ args (apply main-canvas-paint args))]
                    [style '(hscroll vscroll)])])
  (send canvas init-auto-scrollbars (* 25 *tile-size*) (* 25 *tile-size*) 0 0)
  (send frame show #t))
