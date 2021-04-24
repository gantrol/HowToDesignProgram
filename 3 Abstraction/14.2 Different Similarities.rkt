;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |14.2 Different Similarities|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l)
                   (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

; Lon Number -> Lon
; select those numbers on l
; that are below t
(define (small l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(< (first l) t)
        (cons (first l)
          (small
            (rest l) t))]
       [else
        (small
          (rest l) t)])]))

; Lon Number -> Lon
; select those numbers on l
; that are above t
(define (large l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(> (first l) t)
        (cons (first l)
          (large
            (rest l) t))]
       [else
        (large
          (rest l) t)])]))

(check-expect (extract < '() 5) (small '() 5))
(check-expect (extract < '(3) 5) (small '(3) 5))
(check-expect (extract < '(1 6 4) 5)
              (small '(1 6 4) 5))
; Lon Number -> Lon
(define (small-1 l t)
  (extract < l t))
; Lon Number -> Lon
(define (large-1 l t)
  (extract > l t))

; Number Number -> Boolean
; is the area of a square with side x larger than c, and it is usable with extract
(define (squared>? x c)
  (> (* x x) c))

;(extract squared>? (list 3 4 5) 10)

(define 251l (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))
(define 125l (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25))
; Nelon -> Number
; get the func(n) on l
(define (get l func)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (func (first l) (get(rest l ) func))]))
; Nelon -> Number
; determines the smallest 
; number on l
(define (inf1 l)
  (get l min))
; Nelon -> Number
; determines the largest 
; number on l
(define (sup1 l)
  (get l max))

(check-expect (inf1 251l) 1)
(check-expect (inf1 125l) 1)
(check-expect (sup1 251l) 25)
(check-expect (sup1 125l) 25)

; Exercise 246. Check step 1 of the last calculation
;(extract < (cons 6 (cons 4 '())) 5)
;==
;(extract < (cons 4 '()) 5)
