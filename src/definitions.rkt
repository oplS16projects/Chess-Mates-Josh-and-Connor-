#lang racket

(include "board.rkt")
(include "pieces.rkt")
(include "teams.rkt")
(include "tile.rkt")

;multiple definitons
(define horiz-inset 200)
(define vert-inset 35)
(define right-gap 200)
(define bottom-gap 50)
(define horiz 8)
(define vert 8)
(define img-breadth 75)
(define img-length 75)
(define breadth (* horiz img-breadth))
(define length1 (* vert img-length))
(define player "")
(define trash "")