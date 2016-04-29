#lang racket

;; window.rkt
;; Created on 4/23/16 at 3:27 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: provides the window that contains the chess GUI

(require racket/gui)
(require racket/draw)

(require "call.rkt")
(require "board-canvas.rkt")

(provide make-window)

; closure that captures a chess-frame% construction
; and returns it to the caller
(define (make-window
         reset-proc  ; λ -> (button event)
         castle-proc ; λ -> (button event)
         )

  (define chess-frame%
    (class frame%

      ; public method to set output message
      (define/public (set-message string-msg)
        (send output-msg set-label string-msg))
      
      ; public method to set availability of castling button
      (define/public (set-castle-button on?)
        (send castle-button enable on?))

      ; end of chess-frame% definition
      (super-new)))
  
  ; instance of top-level window
  (define frame
    (new chess-frame%
         (label "ChessNuts")
         (width 600)
         (height 600)
         (style (list 'no-resize-border))))
  
  ; horizontal panel to hold output-msg and reset button
  (define panel
    (new horizontal-panel%
         (parent frame)))
  
  ; string representing last message sent to the user
  (define output-msg
    (new message%
         (parent panel)
         (label "Welcome to ChessNuts")
         (min-width 455)
         (font (make-object font% 15 'default))))
  
  ; button for castling
  (define castle-button
    (new button%
         (parent panel)
         (label "Castle")
         (callback castle-proc)
         (enabled #f)))
  
  ; button for reseting the game
  (new button%
       (parent panel)
       (label "Reset")
       (callback reset-proc))

  ; return frame to user
  frame)