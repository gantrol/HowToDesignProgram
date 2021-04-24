;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 257.|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] [X Y -> Y] Y [List-of X] -> Y
; f*oldl works just like foldl
(check-expect (f*oldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
(check-expect (f*oldl / 1 '(6 3 2))
              (foldl / 1 '(6 3 2)))
(define (f*oldl f e l)
  (foldr f e (reverse l)))

; [X] nat [nat -> X] -> [List-of X]
; Constructs a list by applying f to the numbers between 0 and (- n 1):
(check-expect (build-l*st 22 add1)
              (list 1 2 3 4 5 6 7 8 9 10
                    11 12 13 14 15 16 17
                    18 19 20 21 22))
(define (diagonalize i)
    (local ((define (off j)
              (if (= i j) 1 0)))
      (build-list 3 off)))
(check-expect (build-l*st 3 diagonalize)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define (build-l*st n f)
  (reverse (build-reverse-list n f)))

(define (build-reverse-list n f)
  (cond
    [(= n 0) '()]
    [else
     (cons (f (- n 1))
           (build-reverse-list (- n 1) f))]))

; (define (rev l) @ 10.4
; Note on Design Accumulators covers the concepts needed to design these functions from scratch. 