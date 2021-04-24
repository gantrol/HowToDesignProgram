;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |10.1 Functions that Produce Lists（Map）|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (map1 l func)
  (cond
    [(empty? l) '()]
    [else (cons (func (first l))
                (map1 (rest l) func))]))
(define MPH 14)
(check-expect (wage* '()) '())
(check-expect (wage* (list 28)) (list (* 28 MPH)))
(check-expect (wage* (list 4 2)) (list (* 4 MPH) (* 2 MPH)))
(define (wage* whrs)
  (map1 whrs wage))

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* MPH h))

;; Ex 164
; Number -> Number
; $ -> €
(define exchange 0.85)
(define (convert-euro us)
  (* us exchange))
; List-of-numbers -> List-of-numbers
; Converting list of us dollar to eur amount
(check-expect (convert-euro* '()) '())
(check-expect (convert-euro* (list 5)) (list (* 5 exchange)))
(check-expect (convert-euro* (list 78 5)) (list (* 78 exchange) (* 5 exchange)))
(define (convert-euro* lou)
  (map1 lou convert-euro ))

;;; 10.2 Structure in Lists
(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h
(define w1 (make-work "Robby" 11.95 39))
(define w2 (make-work "Amy" 8.88 23))

; Low -> List-of-numbers
; computes the weekly wages for the given records
(check-expect (wage*.v2 '()) '())
(check-expect (wage*.v2 (list w1)) (list (* (work-rate w1) (work-hours w1))))
(check-expect (wage*.v2 (list w2 w1)) (list
                                       (* (work-rate w2) (work-hours w2))
                                       (* (work-rate w1) (work-hours w1))))
(define (wage*.v2 an-low)
  (map1 an-low wage.v2))

; Work -> Number
; computes the wage for work
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

;; Ex 166
(define-struct paycheck [name amount])
; A PayCheck is a structure:
;  (make-paycheck String Number)
;  a paycheck contains the employee’s name and an amount
; Work -> PayCheck
; computes the wage for work, and group work name and wage into paycheck
(define (wage.v3 w)
  (make-paycheck
   (work-employee w)
   (* (work-rate w) (work-hours w))))

(define (wage*.v3 an-low)
  (map1 an-low wage.v3))
;
(define-struct employee.v4 [id name])
(define-struct paycheck.v4 [work amount])
; A PayCheck.v4 is a structure:
;  (make-paycheck Work.v4 Number)
;  a paycheck contains the employee’s name and an amount
; Work -> PayCheck
; computes the wage for work, and group work name and wage into paycheck
(define (wage.v4 w)
  (make-paycheck
   (work-employee w)
   (* (work-rate w) (work-hours w))))

(define (wage*.v4 an-low)
  (map1 an-low wage.v4))

;; Ex 170
(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999.
(define p1 (make-phone 713 568 1234))
(define p2 (make-phone 281 568 1234))
(define p3 (make-phone 555 548 1111))
(check-expect (replace-phone* '()) '())
(check-expect (replace-phone* (list p1)) (list p2))
(check-expect (replace-phone* (list p3 p1)) (list p3 p2))
(define (replace-phone* lop)
  (replace* lop 713 281 replace-phone-area))

(define (replace* l src result func)
  (cond
    [(empty? l) '()]
    [else (cons(func (first l) src result)
              (replace* (rest l) src result func))]))

(define (replace-phone-area item src result)
  (if (= (phone-area item) src)
      (make-phone result (phone-switch item) (phone-four item))
      item))