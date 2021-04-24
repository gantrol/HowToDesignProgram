;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |10.3 Reading files|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(define fn "ttt.txt")
;; Ex 171
; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

(define ttt-lines
  (list "TTT" ""
        "Put up in a place" "where it's easy to see" "the cryptic admonishment" "T.T.T." ""
        "When you feel how depressingly"
        "slowly you climb," "it's well to remember that" "Things Take Time." ""
        "Piet Hein"))
(define ttt-words
  (list "TTT" "Put" "up" "in" "a" "place"
        "where" "it's" "easy" "to" "see" "the" "cryptic" "admonishment"
        "T.T.T."
        "When" "you" "feel" "how" "depressingly" "slowly" "you" "climb,"
        "it's" "well" "to" "remember" "that" "Things" "Take" "Time."
        "Piet" "Hein"))

(check-expect ttt-lines (read-lines fn))
(check-expect ttt-words (read-words fn))

; A List-of-list-of-strings is one of:
; - '()
; - cons(List-of-strings List-of-list-of-strings)
(define ttt-wl
  (list
   (list "TTT")
   '()
   (list "Put" "up" "in" "a" "place")
   (list "where" "it's" "easy" "to" "see")
   (list "the" "cryptic" "admonishment")
   (list "T.T.T.")
   '()
   (list "When" "you" "feel" "how" "depressingly")
   (list "slowly" "you" "climb,")
   (list "it's" "well" "to" "remember" "that")
   (list "Things" "Take" "Time.")
   '()
   (list "Piet" "Hein")))
(check-expect ttt-wl (read-words/line fn))


;

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
; LN -> List-of-numbers
; determines the number of words on each line
(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1)
              (cons 2 (cons 0 '())))

(define (words-on-line lls) '())


;; Ex 172
