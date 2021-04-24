;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |20 Iterative Refinement|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.
(define 127tr.v1
  '("read!" ("part1" "part2" "part3")
            (
              ("hang" "draw")
              (("read!"))
              )))
;; Ex 331
; Dir.v1 -> Number
; count number of file in dir
(check-expect (how-many 127tr.v1) 7)
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [else
     (local
       [(define f (first dir))
        (define r (rest dir))]
       (cond
         [(string? f)
          (+ 1 (how-many r))]
         [else
          (+ (how-many f)
             (how-many r))]))]))
;;;;;;;;;;;;;
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String. 
;;
(define-struct dir [name content])

; TODO: read more dir versions