;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |11.2 Composing Functions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Design one template per data definition.
; Formulate auxiliary function definitions when one data definition points to a second data definition.

;;; 11.3
;; Design a function that sorts a list of reals.

; List-of-number -> List-of-number
; return a lon sorted in descending
(check-expect (sort> '()) '())
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))
(define (sorted>? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [(cons? ne-l)
     (and (> (first ne-l) (first (rest ne-l)))
          (sorted>? (rest ne-l)))]))

(check-satisfied (sort> (list 12 20 -5)) sorted>?)
(define (sort> alon)
  (cond
    [(empty? alon) alon]
    [else (insert (first alon) (sort> (rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
; assume l is arranged in descending order
; the result is sorted in the same way
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(define (insert n alon)
  (cond
    [(empty? alon) (list n)]
    [else (if (< (first alon) n)
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))

