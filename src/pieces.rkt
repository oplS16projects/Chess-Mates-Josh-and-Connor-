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
(require "accum-tiles.rkt")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIECE IMPLEMENTATIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PAWN
(define (make-pawn team tile)
  (define base (make-piece-impl pawn% team tile))

  (define start-y (call (call base 'get-tile) 'get-y))

  (define (draw) 'P)
  
  (define (get-valid-moves board)

    ; helper method takes 'op' argument representing direction of movement
    (define (helper op)
      ; moves default to null
      ; also capture refs to x/y coords
      (let ((moves '())
            (x (call (call base 'get-tile) 'get-x))
            (y (call (call base 'get-tile) 'get-y)))
        
        ; begin accumulating moves
        (begin
          
          ; if y-pos = start-pos, then piece hasn't moved and allow double move
          (if (and (= y start-y)
                   (call (call board 'tile-at x (op y 1)) 'is-empty)
                   (call (call board 'tile-at x (op y 2)) 'is-empty))
              (set! moves (cons (call board 'tile-at x (op y 2)) moves))
              void)
          
          ; check standard 1-space move
          (if (and (not (null? (call board 'tile-at x (op y 1))))
                   (call (call board 'tile-at x (op y 1)) 'is-empty))
              (set! moves (cons (call board 'tile-at x (op y 1)) moves))
              void)
          
          ; check diagonal-right tile for enemy piece to capture
          (let ((t (call board 'tile-at (+ x 1) (op y 1))))
            (if (and (not (null? t))
                     (not (call t 'is-empty))
                     (not (eq? (call (call t 'get-piece) 'get-team) (call base 'get-team))))
                (set! moves (cons t moves))
                void))
          
          ; check diagonal-left tile for enemy piece to capture
          (let ((t (call board 'tile-at (- x 1) (op y 1))))
            (if (and (not (null? t))
                     (not (call t 'is-empty))
                     (not (eq? (call (call t 'get-piece) 'get-team) (call base 'get-team))))
                (set! moves (cons t moves))
                void))
          
          moves)))

    ; call helper method with + or - based on team color
    (helper (if (eq? (call base 'get-team) white-team) - +)))
  
  (define (get-sprite)
    (if (eq? (call base 'get-team) white-team)
        "../Images/White_Pawn.png"
        "../Images/Black_Pawn.png"))
  
  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          ((eq? msg 'get-valid-moves) get-valid-moves)
          ((eq? msg 'get-sprite) get-sprite)
          (else (base msg)))))
  

;; ROOK
(define (make-rook team tile)
  (define base (make-piece-impl rook% team tile))

  (define (draw) 'R)

  (define (get-valid-moves board)
    (accum-straight-tiles board
                          (call base 'get-tile)
                          (call base 'get-team)))

  (define (get-sprite)
    (if (eq? (call base 'get-team) white-team)
        "../Images/White_Rook.png"
        "../Images/Black_Rook.png"))

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          ((eq? msg 'get-valid-moves) get-valid-moves)
          ((eq? msg 'get-sprite) get-sprite)
          (else (base msg)))))

;; BISHOP
(define (make-bishop team tile)
  (define base (make-piece-impl bishop% team tile))

  (define (draw) 'B)

  (define (get-valid-moves board)
    (accum-diagonal-tiles board
                          (call base 'get-tile)
                          (call base 'get-team)))
                          

  (define (get-sprite)
    (if (eq? (call base 'get-team) white-team)
        "../Images/White_Bishop.png"
        "../Images/Black_Bishop.png"))

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          ((eq? msg 'get-valid-moves) get-valid-moves)
          ((eq? msg 'get-sprite) get-sprite)
          (else (base msg)))))

;; KNIGHT
(define (make-knight team tile)
  (define base (make-piece-impl knight% team tile))

  (define (draw) 'T)
  
  (define (get-valid-moves board)
     ; moves default to null....might not need
    (define moves '())

    ; capture refs to X/Y coords....also might not need
    (let ((x (call (call base 'get-tile) 'get-x))
          (y (call (call base 'get-tile) 'get-y)))

      ; begin accumulating moves
      (begin

               ; move down 2 left 1
        (if (and (not (null? (call board 'tile-at (- x 1) (+ y 2))))
                 (call (call board 'tile-at (- x 1) (+ y 2)) 'is-empty))
            (set! moves (cons (call board 'tile-at (- x 1) (+ y 2)) moves))
            void)

               ; move down 2 right 1
        (if (and (not (null? (call board 'tile-at (+ x 1) (+ y 2))))
                 (call (call board 'tile-at (+ x 1) (+ y 2)) 'is-empty))
            (set! moves (cons (call board 'tile-at (+ x 1) (+ y 2)) moves))
            void)

               ; move up 2 left 1
        (if (and (not (null? (call board 'tile-at (- x 1) (- y 2))))
                 (call (call board 'tile-at (- x 1) (- y 2)) 'is-empty))
            (set! moves (cons (call board 'tile-at (- x 1) (- y 2)) moves))
            void)
        
               ; move up 2 right 1
        (if (and (not (null? (call board 'tile-at (+ x 1) (- y 2))))
                 (call (call board 'tile-at (+ x 1) (- y 2)) 'is-empty))
            (set! moves (cons (call board 'tile-at (+ x 1) (- y 2)) moves))
            void)

              ; move left 2 up 1
        (if (and (not (null? (call board 'tile-at (- x 2) (- y 1))))
                 (call (call board 'tile-at (- x 2) (- y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at (- x 2) (- y 1)) moves))
            void)

              ; move left 2 down 1
        (if (and (not (null? (call board 'tile-at (- x 2) (+ y 1))))
                 (call (call board 'tile-at (- x 2) (+ y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at (- x 2) (+ y 1)) moves))
            void)

               ; move right 2 up 1
        (if (and (not (null? (call board 'tile-at (+ x 2) (- y 1))))
                 (call (call board 'tile-at (+ x 2) (- y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at (+ x 2) (- y 1)) moves))
            void)

               ; move right 2 down 1
        (if (and (not (null? (call board 'tile-at (+ x 2) (+ y 1))))
                 (call (call board 'tile-at (+ x 2) (+ y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at (+ x 2) (+ y 1)) moves))
            void))))

  (define (get-sprite)
    (if (eq? (call base 'get-team) white-team)
        "../Images/White_Knight.png"
        "../Images/Black_Knight.png"))

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          ((eq? msg 'get-valid-moves) get-valid-moves)
          ((eq? msg 'get-sprite) get-sprite)
          (else (base msg)))))

;; KING
(define (make-king team tile)
  (define base (make-piece-impl king% team tile))

  (define (draw) 'K)

  (define (get-valid-moves board)

    ; moves default to null....might not need
    (define moves '())

    ; capture refs to X/Y coords....also might not need
    (let ((x (call (call base 'get-tile) 'get-x))
          (y (call (call base 'get-tile) 'get-y)))

      ; begin accumulating moves
      (begin

               ; move down
        (if (and (not (null? (call board 'tile-at x (+ y 1))))
                 (call (call board 'tile-at x (+ y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at x (+ y 1)) moves))
            void)

                ; move left
        (if (and (not (null? (call board 'tile-at (- x 1) y)))
                 (call (call board 'tile-at (- x 1) y) 'is-empty))
            (set! moves (cons (call board 'tile-at (- x 1) y) moves))
            void)
                ; move up
        (if (and (not (null? (call board 'tile-at x (- y 1))))
                 (call (call board 'tile-at x (- y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at x (- y 1)) moves))
            void)
                ; move right
        (if (and (not (null? (call board 'tile-at (+ x 1) y)))
                 (call (call board 'tile-at (+ x 1) y) 'is-empty))
            (set! moves (cons (call board 'tile-at (+ x 1) y) moves))
            void)

                ; down right move
        (if (and (not (null? (call board 'tile-at (+ x 1) (+ y 1))))
                 (call (call board 'tile-at (+ x 1) (+ y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at (+ x 1) (+ y 1)) moves))
            void)
                ; up left move
        (if (and (not (null? (call board 'tile-at (- x 1) (- y 1))))
                 (call (call board 'tile-at (- x 1) (- y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at (- x 1) (- y 1)) moves))
            void)
                ; down left move
        (if (and (not (null? (call board 'tile-at (- x 1) (+ y 1))))
                 (call (call board 'tile-at (- x 1) (+ y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at (- x 1) (+ y 1)) moves))
            void)
                ; up right move
        (if (and (not (null? (call board 'tile-at (+ x 1) (- y 1))))
                 (call (call board 'tile-at (+ x 1) (- y 1)) 'is-empty))
            (set! moves (cons (call board 'tile-at (+ x 1) (- y 1)) moves))
            void))))

  (define (get-sprite)
    (if (eq? (call base 'get-team) white-team)
        "../Images/White_King.png"
        "../Images/Black_King.png"))

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          ((eq? msg 'get-valid-moves) get-valid-moves)
          ((eq? msg 'get-sprite) get-sprite)
          (else (base msg)))))

;; QUEEN
(define (make-queen team tile)
  (define base (make-piece-impl queen% team tile))

  (define (draw) 'Q)

  (define (get-valid-moves board)
    (let ((tl (call base 'get-tile))
          (tm (call base 'get-team)))
      (flatten
       (list (accum-straight-tiles board tl tm)
             (accum-diagonal-tiles board tl tm)))))

  (define (get-sprite)
    (if (eq? (call base 'get-team) white-team)
        "../Images/White_Queen.png"
        "../Images/Black_Queen.png"))

  (λ (msg)
    (cond ((eq? msg 'draw) draw)
          ((eq? msg 'get-valid-moves) get-valid-moves)
          ((eq? msg 'get-sprite) get-sprite)
          (else (base msg)))))