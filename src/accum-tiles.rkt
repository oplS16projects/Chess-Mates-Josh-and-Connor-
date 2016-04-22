#lang racket

;; accum-tiles.rkt
;; Created on 4/21/16 at 8:38 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: provide methods for collecting tiles
;; in a straight or diagonal line into a list. These
;; methods are then used by the different get-valid-moves
;; implementations

(require "call.rkt")

(provide
 accum-diagonal-tiles
 accum-straight-tiles)

;; baseline procedure for accumulating tiles with a given pattern
;; of transforming x and y. Respects line of sight, and makes
;; the distinction between a valid move onto an enemy tile and
;; an invalid move onto a friendly tile
(define (accum-tiles
         x-transform
         y-transform
         board
         start-tile
         team)

  (define (iter tile out-lst)
    (cond
      ; if tile is null or same team, return current list
      ((or (eq? tile '())
           (and (not (call tile 'is-empty))
                (eq? team (call (call tile 'get-piece) 'get-team))))
       out-lst)

      ; if tile contains enemy piece, return current list + current tile
      ((and (not (call tile 'is-empty))
           (not (eq? team (call (call tile 'get-piece) 'get-team))))
       (cons tile out-lst))

      ; otherwise store current tile and make recursive call
      (else
       (let ((x (call tile 'get-x))
             (y (call tile 'get-y)))
         (iter (call board 'tile-at (x-transform x) (y-transform y))
               (cons tile out-lst))))))

  ; baseline call to iter, starts with
  ; a transform because the start tile
  ; is not considered to be a valid move
  (let ((x (call start-tile 'get-x))
        (y (call start-tile 'get-y)))
    (iter (call board 'tile-at (x-transform x) (y-transform y))
          '())))

; shorthand for not transforming the coord
(define (identity n) n)

;; accumulates all diagonal moves for queen and bishop
(define (accum-diagonal-tiles board start-tile team)
  (flatten
   (list
    (accum-tiles (λ (x) (+ x 1)) (λ (y) (+ y 1)) board start-tile team)
    (accum-tiles (λ (x) (+ x 1)) (λ (y) (- y 1)) board start-tile team)
    (accum-tiles (λ (x) (- x 1)) (λ (y) (+ y 1)) board start-tile team)
    (accum-tiles (λ (x) (- x 1)) (λ (y) (- y 1)) board start-tile team))))

;; accumulates all horizontal and veritcal moves for queen and rook
(define (accum-straight-tiles board start-tile team)
  (flatten
   (list
    (accum-tiles (λ (x) (+ x 1)) identity board start-tile team)
    (accum-tiles (λ (x) (- x 1)) identity board start-tile team)
    (accum-tiles identity (λ (y) (+ y 1)) board start-tile team)
    (accum-tiles identity (λ (y) (- y 1)) board start-tile team))))