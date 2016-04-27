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
        (player-turn white-team)
        (winner '()))

    ; method to reset the chess board
    (define (reset-game)
      (begin
        (call board 'reset)
        (set! selected-tile '())
        (set! moused-over-tile '())
        (set! player-turn white-team)
        (set! winner '())
        (print "Game was reset")))

    (define (team-to-string team)
      (if (eq? team white-team)
          "White"
          "Black"))
    
    ; swap to the next player's turn
    (define (swap-turns)
      (begin
        (if (eq? player-turn white-team)
            (set! player-turn black-team)
            (set! player-turn white-team))
        (print (string-append
                (team-to-string player-turn)
                "'s turn"))))
    
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
    ; TODO: send captured pieces to list to be displayed later?
    (define (process-captured-piece piece)
      (unless (null? piece)
        (if (eq? king% (call piece 'get-type))
            (declare-winner (call piece 'get-team))
            void)))
    
    ; sets winner variable based on what team was captured
    (define (declare-winner captured-team)
      (set! winner (if (eq? captured-team white-team)
                       black-team
                       white-team)))
    
    ; procedure for printing a string to the window's output-msg
    (define (print string-msg)
      (send window set-message string-msg))
    
    ; callback for mouse click
    (define (on-mouse-click tile-x tile-y)
      (let ((tile (call board 'tile-at tile-x tile-y)))
        (if (null? winner) (click-tile tile) void)))

    ; callback for mouse over
    (define (on-mouse-over tile-x tile-y)
      (let ((tile (call board 'tile-at tile-x tile-y)))
        (if (and (null? winner) (not (eq? tile moused-over-tile)))
            (begin (send board-canvas refresh-now)
                   (send board-canvas highlight-tile tile-x tile-y "brown")
                   (set! moused-over-tile tile))
            void)))
    
    ; callback for canvas paint
    (define (on-canvas-paint canvas dc)
      (begin
        (map
         (λ (tile) (send canvas draw-tile tile (eq? tile selected-tile)))
         (call board 'get-all-tiles))

        ; mute display if winner is declared
        (unless (null? winner) (send canvas mute-colors))))

    ; callback for the reset button
    (define (on-reset-button button event)
      (reset-game))
    
    ; create window and canvas resources, then present
    ; window to user
    (begin
      (define window (make-window on-reset-button))
      (define board-canvas (make-board-canvas
                            window
                            on-mouse-click
                            on-mouse-over
                            on-canvas-paint))
      (send window show #t))))

(main)
