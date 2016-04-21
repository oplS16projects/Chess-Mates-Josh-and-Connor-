#lang racket

;; pieces.rkt
;; Created on 4/1/16 at 3:10 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: Provides bindings for creating chess pieces,
;; represented by a polymorphic group of classes.

(require "teams.rkt")
(require "tile.rkt")
(require "call.rkt")

(provide
 pawn% rook% bishop% knight% king% queen%
 make-piece)

;; bindings for the individual class symbols, good to have
;; compile-time checking that you typed in the piece name
;; correctly
(define pawn% 'pawn)
(define rook% 'rook)
(define bishop% 'bishop)
(define knight% 'knight)
(define king% 'king)
(define queen% 'queen)

;; factory function for pieces
(define (make-piece type team tile)
  (cond ((eq? type pawn%)
         (make-pawn team tile))
        ((eq? type rook%)
         (make-rook team tile))
        ((eq? type bishop%)
         (make-bishop team tile))
        ((eq? type knight%)
         (make-knight team tile))
        ((eq? type king%)
         (make-king team tile))
        ((eq? type queen%)
         (make-queen team tile))
        (else (error type "Invalid type tag for MAKE-PIECE."))))

;; implementation for the underlying functionality that is common
;; to all chess pieces. This should be thought of as an abstract
;; base class, do NOT instantiate these directly.
(define (make-piece-impl type team tile)

  (define (get-type) type)

  (define (get-team) team)

  (define (get-tile) tile)

  (define (set-tile new-tile)
    (set! tile new-tile))

  (λ (msg)
    (cond ((eq? msg 'get-type) get-type)
          ((eq? msg 'get-team) get-team)
          ((eq? msg 'get-tile) get-tile)
          ((eq? msg 'set-tile) set-tile)
          (else (error "Unrecognized method for" (get-type) ': msg)))))


;;
;; NOTE: these classes are very bare-bones atm, all they can do is draw
;; themselves and call methods inhereted from their base classes.
;;

;; PAWN
(define (make-pawn team tile)
  (define base (make-piece-impl pawn% team tile))

  (define (draw) 'P)

  (define (get-white-moves board)
    (let ((x-pos ((tile 'get-x)))
          (y-pos ((tile 'get-y))))
      (let ((next-y (- y-pos 1)))

        (if (< next-y 0)
            '()
            (list ((board 'tile-at) x-pos next-y))))))

  ; get valid moves for black piece
  (define (get-black-moves board)
    ; moves default to null
    (define moves '())

    ; capture refs to X/Y coords
    (let ((x (call (call base 'get-tile) 'get-x))
          (y (call (call base 'get-tile) 'get-y)))

      ; begin accumulating moves
      (begin
        
        ; if y-pos = 1, then piece hasn't moved and allow double move
        (if (and (= y 1)
                 (call (call board 'tile-at x (+ y 1)) 'is-empty)
                 (call (call board 'tile-at x (+ y 2)) 'is-empty))
            (set! moves (cons (call board 'tile-at x (+ y 2)) moves))
            void)
        
        ; check standard 1-space move
        (if (and (not (null? (call board 'tile-at x (+ y 1))))
                 (call (call board 'tile-at x (+ y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at x (+ y 1)) moves))
            void)

        ; check down-and-left tile for enemy piece to capture
        (let ((t (call board 'tile-at (- x 1) (+ y 1))))
          (if (and (not (null? t))
                   (not (call t 'is-empty))
                   (eq? (call (call t 'get-piece) 'get-team) white-team))
              (set! moves (cons t moves))
              void))

        moves)))

  (define (get-valid-moves board)
    (if (eq? ((base 'get-team)) white-team)
        (get-white-moves board)
        (get-black-moves board)))

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          ((eq? msg 'get-valid-moves) get-valid-moves)
          (else (base msg)))))

;; ROOK
(define (make-rook team tile)
  (define base (make-piece-impl rook% team tile))

  (define (draw) 'R)

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          (else (base msg)))))

;; BISHOP
(define (make-bishop team tile)
  (define base (make-piece-impl bishop% team tile))

  (define (draw) 'B)

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          (else (base msg)))))

;; KNIGHT
(define (make-knight team tile)
  (define base (make-piece-impl knight% team tile))

  (define (draw) 'T)

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          (else (base msg)))))

;; KING
(define (make-king team tile)
  (define base (make-piece-impl king% team tile))

  (define (draw) 'K)

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          (else (base msg)))))

;; QUEEN
(define (make-queen team tile)
  (define base (make-piece-impl queen% team tile))

  (define (draw) 'Q)

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          (else (base msg)))))