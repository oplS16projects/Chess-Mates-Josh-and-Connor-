#lang racket

;; chess-main.rkt
;; Created on 4/23/16 at 3:05 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: file containing entry point for the program

(provide main)

(require "window.rkt")
(require "teams.rkt")
(require "board.rkt")
(require "call.rkt")
(require "move-result.rkt")
(require "pieces.rkt")

(define (main)
  (start-game))

; creates a new game
(define (start-game)
  (let ((board (make-board))
        (selected-tile '())
        (player-turn white-team)
        (print (λ (string-msg) (void))))

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

    ; callback for mouse clicks into the window board
    ; x/y params are in terms of tiles, not pixels
    (define (on-mouse-click tile-x tile-y)
      (let ((tile (call board 'tile-at tile-x tile-y)))
        (click-tile tile)))
          

    (define (on-mouse-over tile-x tile-y)
      void)

    ; create display window and override print procedure to hook into
    ; window's output message
    (set! print (create-window board on-mouse-click on-mouse-over))))

(main)

