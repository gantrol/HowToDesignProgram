;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |14.3 Similarities in Data Definitions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A [List X Y] is a structure: 
;   (cons X (cons Y '()))

; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)
(define elon '())
(define 1-3lon (list 1 2 3))
(define 7-9lon (list 7 8 9))
; A List-numbers-numbers is a structure:
;   (cons List-of-numbers (cons List-of-numbers '()))
(cons 1-3lon (cons 7-9lon '()))
; A List-numbers-1strings is a structure:
;   (cons List-of-numbers (cons List-of-1strings '()))
; A List-string-boolean is a structure:
;   (cons List-of-numbers (cons List-of-1strings '()))

;; Ex 240 Here are two strange but similar data definitions:
; An LStr is [Layer String]
; An LNum is [Layer Number]

; An [Layer ITEM] is one of:
; - ITEM
; - (make-layer ITEM)
(define-struct layer [stuff])

;; Ex 241
; An [NEList-of ITEM] is one of:
; - (cons ITEM '())
; - (cons ITEM [NEList-of ITEM])
; [NEList-of Temperature]

;; Ex 242
; A [Maybe X] is one of: 
; – #false 
; – X

; [List-of String] is one of:
; - '()
; - (cons String [List-of String])

(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(define (occurs s los)
  (cond
    [(empty? los) #false]
    [(cons? los)
     (if (equal? (first los) s)
         (rest los)
         (occurs s (rest los)))]))