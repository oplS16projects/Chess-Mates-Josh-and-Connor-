#lang racket

;; call.rkt
;; Created on 4/19/16 at 11:12 AM
;; Author: Conor Finegan
;; Contact: conor_finegan@student.uml.edu


; export function to other modules and files
(provide call)

; convenient shorthand for calling methods on dispatch objects
; syntax: (call my-obj 'method-name param1 param2 etc)
(define (call obj method . args)
  (apply (obj method) args))