#lang racket

;; board.rkt
;; Created on 4/3/16 at 1:18 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: contains bindings for the "board" class, which
;; is responsible for keeping track of the positions of all
;; pieces on the board.

(require "tile.rkt")
(require "pieces.rkt")
(require "teams.rkt")

(provide make-board)

(define (make-board)
  ;; chess is played on an 8x8 board
  (define board-max 8)

  ;; list of tiles, defaults to null
  (define tiles '())

  ;; returns a tile at the given 
  (define (tile-at x y)
    (list-ref tiles
              (+ (* board-max y) x)))

  ;; method to return flat list of all tiles
  (define (get-all-tiles) tiles)

  ;; initialization function, automatically called
  ;; when make-board is called
  (define (initialize)
    (for ([i board-max])
      (for ([j board-max])
        (set! tiles (cons (make-tile (- board-max 1 j) (- board-max 1 i)) tiles)))))

  ;; reset method, resets all piece positions
  (define (reset)
    (define (reset-tile t)
      (let ((x ((t 'get-x)))
            (y ((t 'get-y)))
            (set-piece (t 'set-piece)))
        (cond ((eq? y 1)  
               (set-piece (make-piece pawn% black-team t))) ; black pawns
              
              ((eq? y 6) 
               (set-piece (make-piece pawn% white-team t))) ; white pawns

              ;; rest of black team
              ((eq? y 0)
               (cond ((or (eq? x 0) (eq? x 7))
                      (set-piece (make-piece rook% black-team t)))
                     ((or (eq? x 1) (eq? x 6))
                      (set-piece (make-piece knight% black-team t)))
                     ((or (eq? x 2) (eq? x 5))
                      (set-piece (make-piece bishop% black-team t)))
                     ((eq? x 3)
                      (set-piece (make-piece queen% black-team t)))
                     ((eq? x 4)
                      (set-piece (make-piece king% black-team t)))))

              ;; rest of white team
              ((eq? y 7)
               (cond ((or (eq? x 0) (eq? x 7))
                      (set-piece (make-piece rook% white-team t)))
                     ((or (eq? x 1) (eq? x 6))
                      (set-piece (make-piece knight% white-team t)))
                     ((or (eq? x 2) (eq? x 5))
                      (set-piece (make-piece bishop% white-team t)))
                     ((eq? x 3)
                      (set-piece (make-piece queen% white-team t)))
                     ((eq? x 4)
                      (set-piece (make-piece king% white-team t)))))

              ;; empty tiles
              (else (set-piece '())))))
    
    (map reset-tile tiles))

  ;; Move-piece method
  ;; TODO: check if move is valid before allowing move
  (define (move-piece orig-tile dest-tile)
    (let ((orig-piece ((orig-tile 'get-piece)))
          (dest-piece ((dest-tile 'get-piece))))

      (if (null? orig-piece)
          "origin tile empty - move aborted"
          (begin
            ((dest-tile 'set-piece) orig-piece)
            ((orig-tile 'set-piece) '())
            "Successfully moved piece"))))

  ;; helper method, draws tile at given X/Y
  (define (draw-tile x y)
    (((tile-at x y) 'draw)))

  ;; draw method (WIP)
  (define (draw)
    (for ([i board-max])
      (for ([j board-max])
        (display (draw-tile j i))
        (display " "))
      (newline)))

  ;; dispatch method
  (define (dispatch msg)
    (cond ((eq? msg 'tile-at) tile-at)
          ((eq? msg 'get-all-tiles) get-all-tiles)
          ((eq? msg 'draw) draw)
          ((eq? msg 'move-piece) move-piece)
          (else (error "Invalid method for BOARD"))))

  ;; when make-board is called, call the initialization
  ;; method, then return the dispatch procedure
  (begin
    (initialize)
    (reset)
    dispatch))
