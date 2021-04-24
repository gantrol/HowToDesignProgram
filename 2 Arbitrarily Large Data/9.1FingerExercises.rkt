;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 9.1FingerExercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Ex138
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)
(define eloa '())
(define 1loa (cons 2.22 eloa))
(define 2loa (cons 441.98 (cons 1923.12 eloa)))
(define 3loa (cons 0 (cons 1 (cons 2 eloa))))

; List-of-amounts -> Positive Double
; computes the sum of the amounts of loa
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else
     (+ (first loa)
        (sum (rest loa)))]))

(check-expect (sum eloa) 0)
(check-expect (sum 1loa) 2.22)
(check-expect (sum 2loa) 2365.1)
(check-expect (sum 3loa) 3)

;; Ex139
; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)
; A Number is one of：
; - Negative Number
; - Nature Number, include 0 and Positive Number
(define elon '())
(define 1lon (cons 2.22 eloa))
(define 1lon1n (cons -2.22 eloa))
(define 2lon (cons 441.98 (cons 1923.12 eloa)))
(define 2lon1n (cons -441.98 (cons 1923.12 eloa)))
(define 2lon2n (cons -441.98 (cons -1923.12 eloa)))

; Number -> Boolean
; if Number is large than or equal to 0, return #true, else false
(define (natural? n)
  (>= n 0))
(check-expect (natural? -123.23) #false)
(check-expect (natural? 0) #true)
(check-expect (natural? 923.23) #true)

; List-of-numbers -> Boolean
; All numbers are positive numbers.
; If (pos? l) yields #true, then l is an element of List-of-amounts

(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [else (and (natural? (first lon))
               (pos? (rest lon)))]))
(check-expect (pos? elon) #true)
(check-expect (pos? 1lon) #true)
(check-expect (pos? 2lon) #true)
(check-expect (pos? 1lon1n) #false)
(check-expect (pos? 2lon1n) #false)
(check-expect (pos? 2lon2n) #false)


;; Ex 140
; A List-of-Boolean is one of:
; - '()
; - (cons Boolean List-of-Boolean)
(define elob '())
(define 1lob (cons #true eloa))
(define 1lob1f (cons #false eloa))
(define 2lob (cons #true 1lob))
(define 2lob2f (cons #true 1lob1f))
(define 2lob1f (cons #false 1lob))
(define 2lob1f2f (cons #false 1lob1f))

; List-of-Boolean -> Boolean
; determines whether all of them are #true
(define (all-true lob)
  (cond
    [(empty? lob) #true]
    [else (and (first lob) (all-true (rest lob)))]))
(check-expect (all-true elob) #true)
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
    [(empty? lob) #false]
    [else (or (first lob) (one-true (rest lob)))]))
(check-expect (one-true elob) #false)
(check-expect (one-true 1lob) #true)
(check-expect (one-true 2lob) #true)
(check-expect (one-true 1lob1f) #false)
(check-expect (one-true 2lob2f) #true)
(check-expect (one-true 2lob1f) #true)
(check-expect (one-true 2lob1f2f) #false)

; List-of-string -> String
; concatenates all strings in l into one long string
 
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect
 (cat (cons "ab" (cons "cd" (cons "ef" '()))))
 "abcdef")
 
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))

;; Ex 142
; ImageOrFalse is one of:
; - Image
; - #false
(require 2htdp/image)
(define ioff #false)
(define eloi '())
(define r/2 5)
(define r (* r/2 2))
(define r1  (rectangle r r/2 "outline" "black"))
(define r2 (rectangle r/2 r "solid" "blue"))
(define s (rectangle r r "solid" "blue"))
(define c (circle r "outline" "red"))

(define loi (cons s eloi))
(define loi1f (cons r1 eloi))
(define loi1f2t (cons r2 loi))
(define loi1t2f (cons c loi1f))
(define loi1f2f (cons r1 (cons r2 eloi)))
(define loidtr (cons s loi))

(define (squareHW? image n)
  (and (= (image-width image) n) (= (image-height image) n)))
; List-Of-Image Number -> ImageOrFalse
; It produces the first image on loi that is not an n by n square
;  if it cannot find such an image, it produces #false.
(check-expect (ill-sized? eloi r) #false)
(check-expect (ill-sized? loi r) s)
(check-expect (ill-sized? loi1t2f r) #false)
(check-expect (ill-sized? loi1f2t r) s)
(check-expect (ill-sized? loidtr r) s)
(check-expect (ill-sized? loi1f r) #false)
(check-expect (ill-sized? loi r/2) #false)
(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (if (squareHW? (first loi) n)
              (first loi)
              (ill-sized? (rest loi) n))]))
