#lang racket

;; chess-main.rkt
;; Created on 4/23/16 at 3:05 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: file containing logic for high-level game state
;; and player swapping

(provide start-game)

(require "tile.rkt")
(require "call.rkt")
(require "board.rkt")
(require "pieces.rkt")
(require "teams.rkt")
(require "window.rkt")
(require "board-canvas.rkt")
(require "move-result.rkt")

; creates a new game
(define (start-game)
  (let ((board (make-board))
        (selected-tile '())
        (selected-valid-moves '())
        (moused-over-tile '())
        (player-turn white-team)
        (winner '()))

    ; method to reset the chess board
    (define (reset-game)
      (begin
        (call board 'reset)
        (set! selected-tile '())
        (set! selected-valid-moves '())
        (set! moused-over-tile '())
        (set! player-turn white-team)
        (set! winner '())
        (print "Game was reset")))

    ; helper procedure to turn team symbol into string
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
    ; has not already been selected, or if the tile contains
    ; a selectable piece. otherwise attempts to move
    (define (click-tile tile )
      (if (or (null? selected-tile)
              (and (not (call tile 'is-empty))
                   (eq? player-turn (call (call tile 'get-piece) 'get-team))))
          (select-tile tile)
          (attempt-move tile)))

    ; deselects a tile
    (define (deselect-tile)
      (set! selected-tile '())
      (set! selected-valid-moves '()))
    
    ; attempts to select a tile, checks to make sure that the
    ; tile is a valid selection for the current player
    (define (select-tile tile)
      (let ((piece (call tile 'get-piece)))
        (if (or (null? piece)
                (not (eq? player-turn (call piece 'get-team))))
            ; if tile is empty or has enemy piece, then it is
            ; not a valid selection
            (begin 
              (print (string-append "Invalid selection for " (team-to-string player-turn)))
              ; not working because it is overriden by on-canvas-paint, need to figure out
              ; a way to marshal this tile into on-canvas-paint
              (send board-canvas highlight-tile tile "red"))
            
            ; else select the tile
            (begin (set! selected-tile tile)
                   (set! selected-valid-moves (call (call tile 'get-piece) 'get-valid-moves board))
                   ;(print "Selected tile")
                   ))))
    
    ; procedure for attempting to move a piece from selected-tile
    ; to dest-tile
    (define (attempt-move dest-tile)
      (let ((result (call board 'move-piece selected-tile dest-tile)))
        (begin
          ;(print (move-result-desc result))
          (deselect-tile)
          (unless (not (move-result-success? result)) (swap-turns))
          (process-captured-piece (move-result-piece result))
          )))

    (define (print-capture-message piece)
      (print (string-append
              (team-to-string (call piece 'get-team)) " "
              (piece-type-to-string (call piece 'get-type))
              " was captured")))
    
    ; declares captured pieces, will also end the game when a king is captured
    ; TODO: send captured pieces to list to be displayed later?
    (define (process-captured-piece0 piece)
        (unless (null? piece)
          (if (eq? king% (call piece 'get-type))
              ; call game if king was captured
              (declare-winner (call piece 'get-team))
              ; else print captured piece
              (print (string-append
                      (team-to-string (call piece 'get-team)) " "
                      (piece-type-to-string (call piece 'get-type))
                      " was captured"))
              )))

    (define (process-captured-piece piece)
      (unless (null? piece)
        (begin
          ; delcare winner if king was captured
          (unless (not (eq? king% (call piece 'get-type)))
            (declare-winner (call piece 'get-team)))
          ; print captured piece message
          (print-capture-message piece))))
                 
    
    ; sets winner variable based on what team was captured
    (define (declare-winner captured-team)
      (set! winner (if (eq? captured-team white-team)
                       black-team
                       white-team))
      (print ""))
    
    ; procedure for printing a string to the window's output-msg
    (define (print string-msg)
      (send window set-message string-msg))
    
    ; callback for mouse click
    (define (on-mouse-click tile-x tile-y)
      (let ((tile (call board 'tile-at tile-x tile-y)))
        (unless (not (null? winner)) (click-tile tile))))

    ; callback for right click
    (define (on-right-click)
      (deselect-tile))

    ; callback for mouse over
    (define (on-mouse-over tile-x tile-y)
      (let ((tile (call board 'tile-at tile-x tile-y)))
        (if (and (null? winner) (not (eq? tile moused-over-tile)))
            (begin (send board-canvas refresh-now)
                   (send board-canvas highlight-tile-pos tile-x tile-y "brown")
                   (set! moused-over-tile tile))
            void)))
    
    ; callback for canvas paint
    (define (on-canvas-paint canvas dc)
      (begin
        ; map over tiles to print
        (map
         (λ (tile) (send canvas draw-tile tile (eq? tile selected-tile)))
         (call board 'get-all-tiles))

        ; highlight valid moves in blue
        (map
         (λ (tile) (send canvas highlight-tile tile "blue"))
         selected-valid-moves)

        ; mute display colors and display winner message
        ; if winner is declared
        (unless (null? winner)
          (begin
            (send canvas mute-colors)
            (send canvas show-winner winner)))))

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
                            on-right-click
                            on-mouse-over
                            on-canvas-paint))
      (send window show #t))))
