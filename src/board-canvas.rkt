#lang racket

;; board-canvas.rkt
;; Created on 4/26/16 at 11:56 AM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: Provides a binding for the canvas on which
;; the board GUI is drawn. This canvas is drawn inside
;; of the main program window.

(require racket/gui)
(require racket/draw)

(require "call.rkt")
(require "teams.rkt")

(provide make-board-canvas)

; closure that captures parameters for board-canvas%
; construction and instantiates a board-canvas% with
; the appropriate parent frame and callback procedures
(define (make-board-canvas
         parent-frame
         mouse-click-proc ; λ -> (tile-x tile-y)
         right-click-proc ; λ -> (void)
         mouse-over-proc  ; λ -> (tile-x tile-y)
         paint-proc       ; λ -> (canvas dc)
         )

  ; constant for canvas size (600 x 600)
  (define canvas-size 600)
  
  ; constant for tile sprite size (75 x 75)
  (define tile-bmp-size 75)

  (define board-canvas%
    (class canvas%
      ; inherit methods from base canvas% class
      (inherit refresh-now)
      (inherit get-dc)
      
      ; tile sprites
      (define white-tile-bmp (read-bitmap "../Images/White_Tile.png"))
      (define black-tile-bmp (read-bitmap "../Images/Black_Tile.png"))
      
      ; method to draw an individual tile
      (define/public (draw-tile tile selected)
        (let ((x (call tile 'get-x))
              (y (call tile 'get-y)))
          (begin
            ; draw tile
            (send (get-dc) draw-bitmap
                  (if (even? (+ x y)) white-tile-bmp black-tile-bmp)
                  (* x tile-bmp-size)
                  (* y tile-bmp-size))
            ; draw piece if applicable
            (let ((piece (call tile 'get-piece)))
              (if (not (null? piece))
                  (draw-piece piece x y)
                  void))
            ; highlight tile if applicable
            (if selected
                (highlight-tile-pos x y "blue")
                void))))

      ; method to highlight a given tile
      (define/public (highlight-tile tile color)
        (let ((tile-x (call tile 'get-x))
              (tile-y (call tile 'get-y)))
          (highlight-tile-pos tile-x tile-y color)))

      ; method to highlight a tile-size rectangle at a given x/y
      (define/public (highlight-tile-pos tile-x tile-y color)
        (begin
          (send (get-dc) set-pen color 3 'solid)
          (send (get-dc) set-brush color 'hilite)
          (send (get-dc) draw-rectangle
                (* tile-x tile-bmp-size)
                (* tile-y tile-bmp-size)
                tile-bmp-size
                tile-bmp-size)))

      ; method to mute the canvas to indicate non-interactivity
      (define/public (mute-colors)
        (begin
          (send (get-dc) set-pen "black" 0 'transparent)
          (send (get-dc) set-brush "black" 'hilite)
          (send (get-dc) draw-rectangle 0 0 canvas-size canvas-size)))

      (define/public (show-winner team)
        (let ((font (make-object font% 40 'modern))
              (msg (string-append (if (eq? team white-team) "White" "Black")
                                  " Wins!")))
          (begin
            (send (get-dc) set-font font)
            (send (get-dc) draw-text msg 130 240))))
      
      ; method to draw an individual piece
      (define (draw-piece piece x y)
        (send (get-dc) draw-bitmap
              (call piece 'get-sprite)
              (* x tile-bmp-size)
              (* y tile-bmp-size)))          
      
      ; mouse event callback
      (define/override (on-event event)
        (let ((mouse-x (send event get-x))
              (mouse-y (send event get-y)))
          (let ((tile-x (floor (/ mouse-x tile-bmp-size)))
                (tile-y (floor (/ mouse-y tile-bmp-size))))
            (cond
              ; if event is left click, call click proc
              ((eq? 'left-down (send event get-event-type))
               (begin (mouse-click-proc tile-x tile-y) (refresh-now)))
              ; if event is right click, call right clikc proc
              ((eq? 'right-down (send event get-event-type))
               (begin (right-click-proc) (refresh-now)))
              ; if mouse leaves window, don't highlight any tile
              ((send event leaving?)
               (refresh-now))
              ; if event is neither of the above but is mouse motion,
              ; call mouse-over-proc
              ((eq? 'motion (send event get-event-type))
               (mouse-over-proc tile-x tile-y))))))
            
      
      ; end of board-canvas% definition
      (super-new)))

  (new board-canvas%
       (parent parent-frame)
       (min-width canvas-size)
       (min-height canvas-size)
       (paint-callback paint-proc)))