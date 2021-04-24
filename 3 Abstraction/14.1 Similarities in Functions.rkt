;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |14.1 Similarities in Functions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define el '())
(define a (list 2))
(define c (list 6 0 2))
;; Ex 236
; Lon, Num -> Lon
; adds n to each item on l
(define (addAll n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n)
              (addAll n (rest l)))]))
; Lon -> Lon
; adds n to each item on l

(define (add1* l)
  (addAll 1 l))
(check-expect (add1* el) el)
(check-expect (add1* a) (list 3))
(check-expect (add1* c) (list 7 1 3))
; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (addAll 5 l))
(check-expect (plus5 el) el)
(check-expect (plus5 a) (list 7))
(check-expect (plus5 c) (list 11 5 7))