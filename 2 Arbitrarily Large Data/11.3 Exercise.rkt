;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |11.3 Exercise|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Ex 139
; List-of-numbers -> Boolean
; if all numbers are positive numbers, return #t; else #f
(check-expect (pos? '()) #t)
(check-expect (pos? (list 1 2 3)) #t)
(check-expect (pos? (list 4 -1 2)) #f)
(check-expect (pos? (list -1 33 4)) #f)
(check-expect (pos? (list 9 23 -1)) #f)
(define (pos? lon)
  (cond
    [(empty? lon) #t]
    [else (and (>= (first lon) 0) (pos? (rest lon)))]))

;(pos?  (cons -1 '()))
;(pos?  (cons 5 '()))

; List-of-number -> [Number, Error]
; produces their sum if the input also belongs to List-of-amounts; otherwise it signals an error.
(define not-all-amount "The list is not all amounts")
(check-error (check-sum (list 3 4 -3)) not-all-amount)
(check-expect (check-sum '()) 0)
(check-expect (check-sum (list 1 2 3)) 6)
(define (check-sum lon)
  (cond
    [(not (check-all lon (lambda (item) (>= item 0)))) (error not-all-amount)]
    [(empty? lon) 0]
    [else (+ (first lon) (check-sum (rest lon)))]))

; List-of[Item], (Func[Item] -> Boolean) -> Boolean
; Check all the items in list is true with func
(define (check-all loi func)
  (cond
    [(empty? loi) #t]
    [else (and (func (first loi))
               (check-all (rest loi) func))]))

;; Ex 187
(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points
(define gp-1 (make-gp "ABC" 33))
(define gp-2 (make-gp "DoUY" 12))
(check-expect (gp-sort '()) '())
(check-expect (gp-sort (list gp-1 gp-2)) (list gp-1 gp-2))
(check-expect (gp-sort (list gp-2 gp-1)) (list gp-1 gp-2))
; descending order
(define (gp-sort l)
  (insert-sort l (lambda (gp1 gp2) (<= (gp-score gp1) (gp-score gp2)))))
;; Ex 188
(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time
(define email-1 (make-email "hjq@aa.a" 234324 "Hello!"))
(define email-2 (make-email "bbb@aa.a" 234324 "Word..."))

(check-expect (email-sort (list email-1 email-2)) (list email-2 email-1))
(check-expect (email-sort '()) '())
(define (email-sort l)
  (insert-sort l (lambda (e1 e2) (string<? (email-from e2) (email-from e1)))))
  

; List-of[Item], (Func[Item, Item]  -> Boolean) -> List-of[Item]
; return a l sorted with func
(define (insert-sort l func)
  (cond
    [(empty? l) l]
    [else (insert (first l) (insert-sort (rest l) func) func)]))
; [Item] List-of-[Item] (Func[Item, Item]  -> Boolean) -> List-of-[Item]
; inserts n into the sorted list of numbers alon
; assume l is arranged in order
(define (insert i loi func)
  (cond
    [(empty? loi) (list i)]
    [else (if(func (first loi) i)
             (cons i loi)
             (cons (first loi) (insert i (rest loi) func)))]))