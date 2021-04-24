;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 8List) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List
; - '()
; - (cons Any List)

; A List-of-names is one of:
; -- '()
; -- (cons String List-of-names)
; interpretation a list of invitees, by last name

; '() a special value, mostly to represent the empty list
; empty? a predicate to recognize '() and nothing else
; cons a checked constructor to create two-field instances
; first the selector to extract the last item added
; rest the selector to extract the extended list
; cons? a predicate to recognize instances of cons

; List-of-names -> Boolean
; 确认"Flatt"是否在a-list-of-names(alon)里
(check-expect (contains-flatt? (cons "Find" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect (contains-flatt? (cons "a" (cons "Flatt" '()))) 
  #true)
(check-expect (contains-flatt? (cons "Flatt" (cons "b" '())))
  #true)
(check-expect (contains-flatt? (cons "a" (cons "b" '())))
  #false)

(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
          (contains-flatt? (rest alon)))]))

; List-of-string, string -> Boolean
; check whether str in alon
(check-expect (contains? '() "a") #false)
(define abc (cons "a" (cons "b" (cons "c" '()))))
(check-expect (contains? abc "a") #true)
(check-expect (contains? abc "b") #true)
(check-expect (contains? abc "c") #true)
(check-expect (contains? abc "d") #false)
(define (contains? alon str)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) str)
         (contains? (rest alon) str))]))


