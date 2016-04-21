#lang racket
;Created by Josh Blanchette on 4/21
;These are things that might be usuful in our main file.  Will most definitely
;add or drop certain procedures along the way
(require graphics/graphics)
(require racket/gui)

;Have includes for all of our files


;Starting board.  Can change the cons with 'B or 'W and 'piece.
(define initialBoard
  (list (list 
         (cons 'B 'rook) (cons 'B 'knight) (cons 'B 'bishop) (cons 'B 'queen) (cons 'B 'king) (cons 'B 'bishop) (cons 'B 'knight) (cons 'B 'rook))
        (list 
         (cons 'B 'pawn) (cons 'B 'pawn)   (cons 'B 'pawn)   (cons 'B 'pawn)  (cons 'B 'pawn) (cons 'B 'pawn)   (cons 'B 'pawn)   (cons 'B 'pawn))
        (build-list 8 (λ(x) '()))
        (build-list 8 (λ(x) '()))
        (build-list 8 (λ(x) '()))
        (build-list 8 (λ(x) '()))
        (list 
         (cons 'W 'pawn) (cons 'W 'pawn)   (cons 'W 'pawn)   (cons 'W 'pawn)  (cons 'W 'pawn) (cons 'W 'pawn)   (cons 'W 'pawn)   (cons 'W 'pawn))
        (list 
         (cons 'W 'rook) (cons 'W 'knight) (cons 'W 'bishop) (cons 'W 'queen) (cons 'W 'king) (cons `W `bishop) (cons `W `knight) (cons 'W 'rook))))

(define board initialBoard)
(define board_copy board)

; Definitions here.  Copied straight from the definitons file...will probably
; delete that file eventually

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

;The window itself is implemented below

(define random-board (open-viewport "Lets play chess!" 800 700))

;listener below

(define (MouseClickListener img)
  '())