#lang racket

;; window.rkt
;; Created on 4/23/16 at 3:27 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: provides the window that contains the chess GUI

(require racket/gui)
(require racket/draw)

(require "board.rkt")
(require "call.rkt")
(require "move-result.rkt")

(provide create-window)

(define (create-window
         board
         mouse-click-proc ; needs to take 2 params (tile-x, tile-y)
         mouse-over-proc  ; needs to take 2 params (tile-x, tile-y)
         )

  ; constant for tile dimension (75x75)
  (define tile-bmp-size 75)
  
  ; top-level window
  (define frame
    (new frame%
         (label "ChessNuts")
         (width 600)
         (height 600)
         (style (list 'no-resize-border))))
  
  ; panel for user controls and output text
  (define output-panel
    (new horizontal-panel%
         (parent frame)))
  
  ; string representing last message sent to user
  (define output-msg
    (new message%
         (parent output-panel)
         (label "No events so far...")))
  
  ; board-canvas is the canvas for the board display
  (define board-canvas%
    (class canvas%
      (inherit refresh-now)

      ; mouse event callback
      (define/override (on-event event)
        (let ((mouse-x (send event get-x))
              (mouse-y (send event get-y)))
          (if (send event button-down?)
              ; if event was button down, call into mouse-click-proc
              (let ((tile-x (floor (/ mouse-x tile-bmp-size)))
                    (tile-y (floor (/ mouse-y tile-bmp-size))))
                (begin (mouse-click-proc tile-x tile-y) (refresh-now)))
              ; else do nothing
              void)))
              
      (super-new)))
  
  ; procedure for drawing the board
  (define (draw-board dc board)
    (let ((white-tile-bmp (read-bitmap "../Images/White_Tile.png"))
          (black-tile-bmp (read-bitmap "../Images/Black_Tile.png")))
      (map
       (λ (tile)
         (let ((x (call tile 'get-x))
               (y (call tile 'get-y)))
           (begin
             ; draw tile
             (send dc draw-bitmap
                   (if (even? (+ x y)) white-tile-bmp black-tile-bmp)
                   (* x tile-bmp-size)
                   (* y tile-bmp-size))
             ; draw piece
             (if (not (call tile 'is-empty))
                 (draw-piece (call tile 'get-piece) dc x y)
                 void))))

       (call board 'get-all-tiles))))
  
  ; method to draw an individual piece
  (define (draw-piece piece dc x y)
    (send dc draw-bitmap
          (call piece 'get-sprite)
          (+ (* tile-bmp-size x) 0)
          (+ (* tile-bmp-size y) 0)))

  ; creates the board-canvas inside frame
  (new board-canvas%
       (parent frame)
       (min-width 600)
       (min-height 600)
       
       ; callback for drawing board
       (paint-callback
        (λ (canvas dc)
          (draw-board dc board))))

  ; show frame
  (send frame show #t)


  ; return simple object for modifying output-msg
  (λ (string-msg)
    (send output-msg set-label string-msg)))
