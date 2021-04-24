;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname UFO) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200) ; distances in terms of pixels 
(define HALF-WIDTH (/ WIDTH 2))
(define HEIGHT 200)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay
             (circle 10 "solid" "black")
             (ellipse 60 30 100 "black"))
)

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game
(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)

 

; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 (define-struct tank [loc vel])

; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; tank maneuvering into position to fire the missile
(make-aim (make-posn 20 10) (make-tank 28 -3))
; missile has been fired
(make-fired (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- HEIGHT TANK-HEIGHT)))
; missile is about to collide with the UFO
(make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103))
