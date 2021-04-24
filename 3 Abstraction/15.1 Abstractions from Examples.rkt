;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |15.1 Abstractions from Examples|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Ex 250

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(check-expect (tab-sin 0) (list 0))
(define (tab-sin n)
  (tabulate n sin))
  

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(check-expect (tab-sqrt 1) (list 1 0))
(define (tab-sqrt n)
  (tabulate n sqrt))

; Number Func -> [List-of Number]
; tabulates func between n 
; and 0 (incl.) in a list
(define (tabulate n func)
  (cond
    [(= n 0) (list (func 0))]
    [else
     (cons
      (func n)
      (tabulate (sub1 n) func))]))
;; Ex 251

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(check-expect (sum (list 0 1 2 3 4)) 10)
(define (sum l)
  (fold1 l + 0))
  

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(check-expect (product (list 1 2 3 4)) 24)
(define (product l)
  (fold1 l * 1))
(define (fold1 l func base)
  (cond
    [(empty? l) base]
    [else
     (func (first l)
           (fold1 (rest l) func base))]))


;; Ex 252
(require 2htdp/image)
(define (fold2 l func base)
  (cond
    [(empty? l) base]
    [else
     (func (first l)
           (fold2 (rest l) func base))]))
  
; [List-of Number] -> Number
(check-expect (product2 (list 1 2 3 4)) 24)
(define (product2 l)
  (fold2 l * 1))
  
; [List-of Posn] -> Image
(check-expect (image* (list (make-posn 33 44))) (place-image dot 33 44 emt))
(define (image* l)
  (fold2 l place-dot emt))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))
