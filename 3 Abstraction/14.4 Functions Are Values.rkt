;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |14.4 Functions Are Values|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Ex 245
; (Func Number Number), (Func Number nubmer) -> Boolean
; whether the two produce the same results for 1.2, 3, and -5.775.
(define (f x) x)
(define (fy y) y)
(define (fy2 y) (* 2 y))
(check-expect (function=at-1.2-3-and-5.775? f fy) #t)
(check-expect (function=at-1.2-3-and-5.775? fy fy2) #f)
(define (function=at-1.2-3-and-5.775? f1 f2)
  (and
   (equal? (f1 1.2) (f2 1.2))
   (equal? (f1 3) (f2 3))
   (equal? (f1 5.775) (f2 5.775))))