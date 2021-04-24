;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket
(provide (all-defined-out))

; Helper Functions
(define (string-first s)
  (substring s 0 1))
(define (string-rest s)
  (cond
    [(= (string-length s) 0) ""]
    [else (substring s 1)]))
(define (string-last s)
  (substring s (- (string-length s) 1)))
(define (string-remove-last s)
  (cond
    [(= (string-length s) 0) ""]
    [else (substring s 0 (- (string-length s) 1))]))
  