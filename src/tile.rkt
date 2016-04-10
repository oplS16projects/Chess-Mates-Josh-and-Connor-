#lang racket

;; tile.rkt
;; Created on 4/1/16 at 2:55 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: Provides the "tile" class, which represents
;; an individual tile on the 8x8 chess board. "tile"
;; provides functions for retrieving its own X and Y
;; coordinates on the board, the chess piece on it (if
;; any), and the board owning the tile.


;; make-tile is the only public function in this module
(provide make-tile)

;; constructor for "tile" class
(define (make-tile x y)

  ;; piece member variable
  (define piece '())

  ;; getter method for X coord
  (define (get-x) x)

  ;; getter method for Y coord
  (define (get-y) y)

  ;; getter method for the piece on this tile
  (define (get-piece) piece)

  ;; setter method for the piece on this tile
  ;; allows for removing and adding of piecs
  ;; by convention, a null piece should be '()
  (define (set-piece new-piece)
    (set! piece new-piece))

  ;; draw method (WIP)
  (define (draw)
    (if (null? piece)
        " "
        ((piece 'draw))))

  ;; dispatch
  (λ (msg)
    (cond ((eq? msg 'get-x) get-x)
          ((eq? msg 'get-y) get-y)
          ((eq? msg 'get-piece) get-piece)
          ((eq? msg 'set-piece) set-piece)
          ((eq? msg 'draw) draw)
          (else (error msg "Invalid method for TILE")))))