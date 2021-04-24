;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |9.3 Natural Numbers|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 150. Design the function add-to-pi.
; N -> Number
; computes (+ n pi) without using +
 
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (add-helper n pi))
; N, Number -> Number
; help func for add-to-pi
(define (add-helper n num)
  (helper n num add1))
  
(define (helper n num func)
  (cond
    [(zero? n) num]
    [else (helper (sub1 n) (func num) func)]))
;; Ex 151
; N, Number -> Number
; n * num
(check-expect (multiply 3 5) 15)
(define (multiply n num)
  (mult-helper n num 0))
(define (mult-helper n num temp)
  (cond
    [(zero? n) temp]
    [else (mult-helper (sub1 n) num (add-helper num temp))]))


