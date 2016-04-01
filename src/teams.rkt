#lang racket

;; teams.rkt
;; Created on 4/1/16 at 2:40 PM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu
;; Purpose: Provides bindings that act as an enumeration,
;;          allowing for easy distinction between the white team
;;          and black team.


;; Public exports
(provide
 black-team
 white-team
 team?
 )

;; Binding for the black team (team that goes second)
(define black-team 'black-team)

;; Binding for the whit eteam (team that goes first)
(define white-team 'white-team)

;; Returns #t if tm is a team, #f otherwise
(define (team? tm)
  (or (eq? tm black-team)
      (eq? tm white-team)))