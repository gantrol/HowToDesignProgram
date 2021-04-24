;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Family Tree|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)
; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> ???
(define (fun-FT an-ftree)
  (cond
    [(no-parent? an-ftree) ...]
    [else (... (fun-FT (child-father an-ftree)) ...
           ... (fun-FT (child-mother an-ftree)) ...
           ... (child-name an-ftree) ...
           ... (child-date an-ftree) ...
           ... (child-eyes an-ftree) ...)]))


; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (blue-eyed-child?
               (child-father an-ftree))
              (blue-eyed-child?
               (child-mother an-ftree))
              (equal? "blue" (child-eyes an-ftree)))]))

(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)


;; Ex 310
; FT -> Number
; consumes a family tree and counts the child structures in the tree.
(check-expect (count-persons NP) 0)
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)
(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (count-persons (child-father an-ftree))
             (count-persons (child-mother an-ftree))
             1)]))
;; Ex 311
; FT Year -> Number
; Computer the average age in y
(check-expect (average-age NP 2000) 0)
(check-expect (average-age Bettina 2000) 74)
(check-expect (average-age Gustav 2000) 45.8)
(define (average-age an-ftree y)
  (cond
    [(no-parent? an-ftree) 0]
    [else (/ (sum-age an-ftree y) (count-persons an-ftree))]))
; FT Year -> Number
; sum of age of an-ftree by y
(define (sum-age an-ftree y)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (sum-age (child-father an-ftree) y)
             (sum-age (child-mother an-ftree) y)
             (- y (child-date an-ftree)))]))
;; Ex 312
; FT -> List-of-eys-colors
