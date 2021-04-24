;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |16.1 Existing Abstractions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 256. Explain the following abstract function:
; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
(define single (list 8))
(define l (list 1 2 3 5 4))
(define strs (list "z" "a" "g"))
(check-expect (argmax1 (lambda (x) x) l) 5)
(check-expect (argmax1 string->int strs) "z")
(define (argmax1 f lx)
  (arg1 f lx >=))

; [X] [X -> Number] [NEList-of X] -> X
; finds the (first) item in lx that minimizes f
(check-expect (argmin1 (lambda (x) x) l) 1)
(check-expect (argmin1 string->int strs) "a")
(define (argmin1 f lx)
  (arg1 f lx <=))

; [X] [X -> Number] [NEList-of X] [X X-> Boolean]
; -> X 
(define (arg1 f lx compare)
  (cond
    [(empty? (rest lx)) (first lx)]
    [else
     (local
       [(define rest-m (arg1 f (rest lx) compare))
        (define first-lx (first lx))]
       (if (compare (f first-lx) (f rest-m))
           first-lx
           rest-m))]))
