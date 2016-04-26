#lang racket

;; chess-main.rkt
;; Created on 4/23/16 at 3:05 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: file containing entry point for the program

(provide main)

(require "call.rkt")
(require "board.rkt")
(require "pieces.rkt")
(require "teams.rkt")
(require "window.rkt")
(require "board-canvas.rkt")
(require "move-result.rkt")

(define (main)
  (start-game))

; creates a new game
(define (start-game)
  (let ((board (make-board))
        (selected-tile '())
        (moused-over-tile '())
        (player-turn white-team))
    
    ; swap to the next player's turn
    (define (swap-turns)
      (if (eq? player-turn white-team)
          (set! player-turn black-team)
          (set! player-turn white-team)))
    
    ; when given a tile, attempt a selection if a piece
    ; has not already been selected. otherwise, attempt
    ; a move.
    (define (click-tile tile )
      (if (null? selected-tile)
          (select-tile tile)
          (attempt-move tile)))
    
    ; attempts to select a tile, checks to make sure that the
    ; tile is a valid selection for the current player
    (define (select-tile tile)
      (let ((piece (call tile 'get-piece)))
        (if (or (null? piece)
                (not (eq? player-turn (call piece 'get-team))))
            ; if tile is empty or has enemy piece, then it is
            ; not a valid selection
            (print "Selection invalid")
            ; else select the tile
            (begin (set! selected-tile tile)
                   (print "Selected tile")))))
    
    ; procedure for attempting to move a piece from selected-tile
    ; to dest-tile
    (define (attempt-move dest-tile)
      (let ((result (call board 'move-piece selected-tile dest-tile)))
        (begin
          (print (move-result-desc result))
          (set! selected-tile '())
          (process-captured-piece (move-result-piece result))
          (if (move-result-success? result) (swap-turns) void))))
    
    ; for now only delcares a winner, doesn't do anyting else 
    (define (process-captured-piece piece)
      (if (null? piece)
          void
          (if (eq? king% (call piece 'get-team))
              (declare-winner (call piece 'get-team))
              void)))
    
    ; broken? won't print winner msg
    (define (declare-winner captured-team)
      (let ((winner-msg (string-append
                         (if (eq? captured-team black-team)
                             "White player" "Black player")
                         " is the winner!")))
        (print winner-msg)))
    
    ; procedure for printing a string to the window's output-msg
    (define (print string-msg)
      (send window set-message string-msg))
    
    ; callback for mouse click
    (define (on-mouse-click tile-x tile-y)
      (let ((tile (call board 'tile-at tile-x tile-y)))
        (click-tile tile)))

    ; callback for mouse over
    (define (on-mouse-over tile-x tile-y)
      (let ((tile (call board 'tile-at tile-x tile-y)))
        (if (not (eq? tile moused-over-tile))
            (begin (send board-canvas refresh-now)
                   (send board-canvas highlight-tile tile-x tile-y "brown")
                   (set! moused-over-tile tile))
            void)))
    
    ; callback for canvas paint
    (define (on-canvas-paint canvas dc)
      (map
       (λ (tile) (send canvas draw-tile tile (eq? tile selected-tile)))
       (call board 'get-all-tiles)))
    
    ; create window and canvas resources, then present
    ; window to user
    (begin
      (define window (make-window))
      (define board-canvas (make-board-canvas
                            window
                            on-mouse-click
                            on-mouse-over
                            on-canvas-paint))
      (send window show #t))))

(main)
