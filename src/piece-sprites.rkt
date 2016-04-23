#lang racket

;; piece-sprites.rkt
;; Created on 4/23/16 at 4:15 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: provide bindings for the sprites
;; representing the chess pieces

(require racket/draw)

(provide
 black-pawn-bmp
 white-pawn-bmp
 black-rook-bmp
 white-rook-bmp
 black-bishop-bmp
 white-bishop-bmp
 black-knight-bmp
 white-knight-bmp
 black-king-bmp
 white-king-bmp
 black-queen-bmp
 white-queen-bmp)

(define black-pawn-bmp (read-bitmap "../Images/Black_Pawn.png"))
(define white-pawn-bmp (read-bitmap "../Images/White_Pawn.png"))

(define black-rook-bmp (read-bitmap "../Images/Black_Rook.png"))
(define white-rook-bmp (read-bitmap "../Images/White_Rook.png"))

(define black-bishop-bmp (read-bitmap "../Images/Black_Bishop.png"))
(define white-bishop-bmp (read-bitmap "../Images/White_Bishop.png"))

(define black-knight-bmp (read-bitmap "../Images/Black_Knight.png"))
(define white-knight-bmp (read-bitmap "../Images/White_Knight.png"))

(define black-king-bmp (read-bitmap "../Images/Black_King.png"))
(define white-king-bmp (read-bitmap "../Images/White_King.png"))

(define black-queen-bmp (read-bitmap "../Images/Black_Queen.png"))
(define white-queen-bmp (read-bitmap "../Images/White_Queen.png"))

