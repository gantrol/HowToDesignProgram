;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |9.2 Non-empty Lists|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 

(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

(check-expect
  (average (cons 1 (cons 2 (cons 3 '())))) 2)

; NEList-of-temperatures -> Number
; computes the average temperature 
(define (average alot)
  (/ (sum alot) (how-many alot)))
 
; List-of-temperatures -> Number 
; adds up the temperatures on the given list 
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [(cons? alot)
     (+ (first alot) (sum (rest  alot)))]))
 
; List-of-temperatures -> Number 
; counts the temperatures on the given list 
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [(cons? alot)
     (+ 1 (how-many (rest alot)))]))

;; Ex 147
(define 12l (cons 1
  (cons 2
    '()))
)
(define 32l (cons 3
  (cons 2
    '()))
)
(define 032l (cons 0
  (cons 3
    (cons 2
      '())))
)
; NEList-of-temperatures -> Boolean
; true if the temperatures are sorted in descending orderin
(check-expect (sorted>? (cons 1 '())) #true)
(check-expect (sorted>? 12l) #false)
(check-expect (sorted>? 32l) #true)
(check-expect (sorted>? 032l) #false)
(define (sorted>? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [(cons? ne-l)
     (and (> (first ne-l) (first (rest ne-l)))
          (sorted>? (rest ne-l)))]))

;; Ex 147 
; A NEList-of-Booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-Boolean)
(define elob '())
(define 1lob (cons #true elob))
(define 1lob1f (cons #false elob))
(define 2lob (cons #true 1lob))
(define 2lob2f (cons #true 1lob1f))
(define 2lob1f (cons #false 1lob))
(define 2lob1f2f (cons #false 1lob1f))

; NEList-of-Boolean -> Boolean
; determines whether all of them are #true
(define (all-true lob)
  (cond
    [(empty? (rest lob)) (first lob)]
    [else (and (first lob) (all-true (rest lob)))]))
(check-expect (all-true 1lob) #true)
(check-expect (all-true 2lob) #true)
(check-expect (all-true 1lob1f) #false)
(check-expect (all-true 2lob2f) #false)
(check-expect (all-true 2lob1f) #false)
(check-expect (all-true 2lob1f2f) #false)

; List-of-Boolean -> Boolean
; determines whether at least one of them are #true
(define (one-true lob)
  (cond
    [(empty? (rest lob)) (first lob)]
    [else (or (first lob) (one-true (rest lob)))]))
(check-expect (one-true 1lob) #true)
(check-expect (one-true 2lob) #true)
(check-expect (one-true 1lob1f) #false)
(check-expect (one-true 2lob2f) #true)
(check-expect (one-true 2lob1f) #true)
(check-expect (one-true 2lob1f2f) #false)