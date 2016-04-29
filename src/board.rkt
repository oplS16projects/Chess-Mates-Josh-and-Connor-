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
(require "call.rkt")
(require "move-result.rkt")

(provide make-board board-max)

;; chess is played on an 8x8 board
(define board-max 8)

(define (make-board)
  ;; list of tiles, defaults to null
  (define tiles '())

  ;; returns a tile at the given X/Y coord
  ;; returns '() for an invalid coord
  ;; this silent, soft failure is useful for
  ;; methods that accumulate the tiles around
  ;; a piece - these methods will more easily be able to
  ;; deal with board boundries.
  (define (tile-at x y)
    (if (or (>= x board-max)
            (>= y board-max)
            (< x 0) (< y 0))
        '()
        (list-ref tiles
                  (+ (* board-max y) x))))

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

  ;; move-piece method, takes 2 tiles as arguments and
  ;; attempts to move the piece from the former tile
  ;; to the latter tile. This method should only allow the
  ;; move complete if it abides by the rules of chess, but
  ;; it should never error. Returns a data structure
  ;; representing the results of the move, including a string
  ;; representing success or failure and a reference to any piece
  ;; removed from the board because of this move.
  (define (move-piece orig-tile dest-tile)
    
    ;; Fail loudly if either tile is null
    (cond ((null? orig-tile) (error "Invalid orig-tile"))
          ((null? dest-tile) (error "Invalid dest-tile"))
          (else

           ;; Store references to the pieces on each tile
           (let ((orig-piece ((orig-tile 'get-piece)))
                 (dest-piece ((dest-tile 'get-piece))))

             ;; Return failure if you tried to move from an empty tile
             (if (null? orig-piece)
                 (make-move-result "Origin tile empty - move aborted" '() #f)

                 ;; Otherwise check for validity of move
                 (let ((valid-moves ((orig-piece 'get-valid-moves) dispatch)))
                   (if (not (eq? #f (member dest-tile valid-moves)))
                       ;; If move is valid, perform move
                       (begin
                         (call dest-tile 'set-piece orig-piece)
                         (call orig-piece 'set-tile dest-tile)
                         (call orig-tile 'set-piece '())
                         (make-move-result "Successfully moved piece" dest-piece #t))

                       ;; Else return failure
                       (make-move-result "Selection was not valid - move aborted" '() #f))))))))

  ;; Forces a piece to move from orig-tile to dest-tile.
  ;; !!! This method is primarily meant for debug purposes !!!
  ;; It does not check for the validity of the move being
  ;; made. Similarly to move-piece, force-move-piece should
  ;; never error, but rather returns a data structure representing
  ;; the result of hte move, including a string representing success
  ;; or failure and a reference to any piece removed from the board
  ;; because of this move.
  (define (force-move-piece orig-tile dest-tile)

    ;; Fail loudly if either tile is null
    (cond ((null? orig-tile) (error "Invalid orig-tile"))
          ((null? dest-tile) (error "Invalid dest-tile"))
          (else

           ;; Store reference to the pieces on each tile
           (let ((orig-piece (call orig-tile 'get-piece))
                 (dest-piece (call dest-tile 'get-piece)))

             ;; Return failure if you tried to move from an empty tile
             (if (null? orig-piece)
                 (make-move-result "Origin tile empty - move aborted" '() #f)

                 ;; Otherwise perform move
                 (begin
                   (call dest-tile 'set-piece orig-piece)
                   (call orig-piece 'set-tile dest-tile)
                   (call orig-tile 'set-piece '())
                   (make-move-result "Force-moved piece" dest-piece #t)))))))

  ;; helper method, draws tile at given X/Y
  (define (draw-tile x y)
    (((tile-at x y) 'draw)))

  ;; draw method (only used for debugging, prints
  ;; ASCII board in REPL
  (define (draw)
    (for ([i board-max])
      (for ([j board-max])
        (display (draw-tile j i))
        (display " "))
      (newline)))
  
  ; method for castling
  ; doesn't check for validity, just force-moves
  ; the appropriate tiles
  (define (castle team)
    
    ; helper method for castling black team
    (define (castle-black)
      (begin
        (force-move-piece (tile-at 4 0) (tile-at 6 0))
        (force-move-piece (tile-at 7 0) (tile-at 5 0))))
    
    ; helper method for castling white team
    (define (castle-white)
      (begin
        (force-move-piece (tile-at 4 7) (tile-at 6 7))
        (force-move-piece (tile-at 7 7) (tile-at 5 7))))
    
    ; call appropriate helper
    (if (eq? team white-team)
        (castle-white)
        (castle-black)))
  
  ; method to determine if castling is valid
  ; for a given team
  (define (can-castle? team)
    (let ((y (if (eq? team white-team) 7 0)))
      (let ((k-piece (call (tile-at 4 y) 'get-piece))
            (r-piece (call (tile-at 7 y) 'get-piece))
            (t-piece (call (tile-at 6 y) 'get-piece))
            (b-piece (call (tile-at 5 y) 'get-piece)))
        (and (not (null? k-piece))
             (not (null? r-piece))
             (not (call k-piece 'has-moved))
             (not (call r-piece 'has-moved))
             (null? t-piece)
             (null? b-piece)))))

  ; method to calculate whether the defending-team is in check
  ; if the defending team is in check, returns a list with the
  ; defending king at the head, followed by all tiles with valid
  ; moves to his tile
  ; if the defending team is not in check, returns '()
  (define (find-check defending-team)

    ; helper procedure to get all pieces NOT of defending-team color
    (define (not-defending-pred tile)
      (let ((piece (call tile 'get-piece)))
        (and (not (null? piece))
             (not (eq? defending-team (call piece 'get-team))))))

    ; helper procedure to find defending king
    (define (defending-king-pred tile)
      (let ((piece (call tile 'get-piece)))
        (and (not (null? piece))
             (eq? king% (call piece 'get-type))
             (eq? defending-team (call piece 'get-team)))))

    (let ((attacking-tiles (filter not-defending-pred (get-all-tiles)))
          (defending-king (car (filter defending-king-pred (get-all-tiles)))))

      ; helper procedure to find if a piece can reach the king
      (define (can-reach-king-pred tile)
        (let ((valid-moves (call (call tile 'get-piece) 'get-valid-moves dispatch)))
          (not (eq? #f (member defending-king valid-moves)))))

      (let ((dangerous-moves (filter can-reach-king-pred attacking-tiles)))
        (if (null? dangerous-moves)
            '()
            (cons defending-king dangerous-moves)))))
              
          

  ;; dispatch method
  (define (dispatch msg)
    (cond ((eq? msg 'tile-at) tile-at)
          ((eq? msg 'get-all-tiles) get-all-tiles)
          ((eq? msg 'draw) draw)
          ((eq? msg 'move-piece) move-piece)
          ((eq? msg 'force-move-piece) force-move-piece)
          ((eq? msg 'reset) reset)
          ((eq? msg 'castle) castle)
          ((eq? msg 'can-castle) can-castle?)
          ((eq? msg 'find-check) find-check)
          (else (error "Invalid method for BOARD"))))

  ;; when make-board is called, call the initialization
  ;; method, then return the dispatch procedure
  (begin
    (initialize)
    (reset)
    dispatch))

;(define b (make-board))
;
;(call b 'force-move-piece (call b 'tile-at 4 0) (call b 'tile-at 4 4))
;(call b 'force-move-piece (call b 'tile-at 3 7) (call b 'tile-at 7 4))
;
;((b 'draw))
;
;(call b 'find-check black-team)