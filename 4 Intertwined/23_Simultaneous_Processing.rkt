;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 23_Simultaneous_Processing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; Case 1
; one of the arguments may have to be treated as if it were atomic.
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
                (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons (first front)
           (replace-eol-with (rest front) end))]))

;; Excercise 387
; [List of Symbol] [List of Number] -> [List of '(Symbol Number)]
; produces all possible ordered pairs of symbols and numbers.
(check-expect (cross '() '(1 2)) '())
(check-expect (cross '(a b c) '()) '())
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else (local [(define (sb-cross-lon sb lon)
                    (cond
                      [(empty? lon) '()]
                      [else
                       (cons (list sb (first lon))
                        (sb-cross-lon sb (rest lon)))]))]
            (append (sb-cross-lon (first los) lon)
                  (cross (rest los) lon)))]))
;;; Case 2
; the function must process the two arguments in lockstep
; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length 
(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))
(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons
      (weekly-wage (first hours) (first wages/h))
      (wages*.v2 (rest hours) (rest wages/h)))]))
; Number Number -> Number
; computes the weekly wage from pay-rate and hours
; 看起来这个函数没什么用,但这部分功能是很可能改动的——收税、五险一金……
; 如果要加上这些因素，或者这些因素有变动，只要改动这个函数即可
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

;; Exercise 388
(define-struct employee(name ssn rate))
(define-struct work(name hours))
(define-struct nwage(name wage))
; wages*.v2 consumes lists of employee structures and lists of work records.
; return list of structures that contain the name of the employee and the weekly wage.
(define e1 (make-employee "eee1" "21345" 1000))
(define e2 (make-employee "eee2" "21346" 2000))
(define e3 (make-employee "eee3" "21347" 3000))
(define es (list e1 e2 e3))
(define w1 (make-work "eee1" 1))
(define w2 (make-work "eee2" 2))
; 暂不考虑多条记录的问题，略微复杂
;(define w22 (make-work "eee2" "1"))
(define w3 (make-work "eee3" 3))
(define ws (list w1 w2 w3))
(define nw1 (make-nwage "eee1" 1000))
(define nw2 (make-nwage "eee2" 4000))
(define nw3 (make-nwage "eee3" 9000))
(define nws (list nw1 nw2 nw3))
(check-expect (wages*.v3 es ws) nws)
; assume the two lists are of equal length 
(define (wages*.v3 loe low)
  (cond
    [(empty? loe) '()]
    [else
     (cons (wage.v3 (first loe) (first low))
         (wages*.v3 (rest loe) (rest low)))]))

; Employee Work -> Nwage
(define (wage.v3 e w)
  (make-nwage (employee-name e)
              (weekly-wage (employee-rate e) (work-hours w)))) 


;; Exercise 389.

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; [List String] [List String] -> [List phone-record]
; consumes a list of names and a list of phone numbers and return list of phone record
; assumes the two list are of same length
(check-expect (zip (list "n1" "n2") (list "2222" "1111111"))
              (list (make-phone-record "n1" "2222") (make-phone-record "n2" "1111111")))
(define (zip lon lopn)
  (cond
    [(empty? lon) '()]
    [else
     (cons (make-phone-record (first lon) (first lopn))
         (zip (rest lon) (rest lopn)))]))
;;; Case 3
; process the given data in accordance to all possible cases

; N is one of: 
; – 0
; – (add1 N)

; [List Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol
(define (list-pick l n)
  'a)
