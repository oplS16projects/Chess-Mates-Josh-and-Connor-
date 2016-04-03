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

(provide
 pawn% rook% bishop% knight% king% queen%
 make-piece)

(define pawn% 'pawn)
(define rook% 'rook)
(define bishop% 'bishop)
(define knight% 'knight)
(define king% 'king)
(define queen% 'queen)

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
          (else (error "Unrecognized method for" (get-type) ': msg)))))

;; PAWN
(define (make-pawn team tile)
  (define base (make-piece-impl pawn% team tile))

  (define (draw) 'P)

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
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