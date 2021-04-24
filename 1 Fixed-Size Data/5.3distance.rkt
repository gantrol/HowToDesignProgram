;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 5.3distance) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define a-p (make-posn 8 6))

;posn -> number
;computes the distance of ap to the origin
;(check-expect (distance-to-0 (make-posn 0 5)) 5)
;(check-expect (distance-to-0 (make-posn 7 0)) 7)
(check-expect (distance-to-0 (make-posn 3 4)) 5)
;(check-expect (distance-to-0 (make-posn 8 6)) 10)
;(check-expect (distance-to-0 (make-posn 5 12)) 13)
(define (distance-to-0 ap)
  (sqrt (+ (sqr (posn-x ap)) (sqr (posn-y ap)))))
;(distance-to-0 (make-posn 6 (* 2 4)))
;(+ (distance-to-0 (make-posn 12 5)) 10)

;posn -> number
; computes the manhattan-distance of ap to the origin
(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn 5 0)) 5)
(check-expect (manhattan-distance (make-posn 0 2)) 2)
(check-expect (manhattan-distance (make-posn 55 4)) 59)
(define (manhattan-distance ap)
  (+ (posn-x ap)
      (posn-y ap)))