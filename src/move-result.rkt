#lang racket

;; move-result.rkt
;; Created on 4/19/16 at 11:24 AM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;;
;; Purpose: exposes a data structure representing the
;; result of a piece movement. This structure contains
;; a string that can be used for debugging, user interface,
;; or both, as well as a reference to the piece removed from
;; the board as a result of this move (if applicable). If no
;; piece was removed from the board, then the piece field
;; contains a nil refence '()

(provide
 make-move-result
 move-result-desc
 move-result-piece
 move-result-success?)

(define (make-move-result desc piece success)
  (cons desc (cons piece success)))

(define (move-result-desc mr)
  (car mr))

(define (move-result-piece mr)
  (car (cdr mr)))

(define (move-result-success? mr)
  (cdr (cdr mr)))