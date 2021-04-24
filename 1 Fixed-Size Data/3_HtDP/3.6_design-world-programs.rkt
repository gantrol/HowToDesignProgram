;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname design-world-programs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;# Constants
;## Physical constants

 
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 2))
(define WIDTH-OF-CAR-BODY (+ WHEEL-DISTANCE (* WHEEL-RADIUS 6)))
(define HEIGHT-OF-CAR-BODY (* WHEEL-RADIUS 2))
(define WIDTH-OF-CAR-HEAD (/ WIDTH-OF-CAR-BODY 2))
(define HEIGHT-OF-CAR-HEAD (* 3 (/ HEIGHT-OF-CAR-BODY 4)))
(define WIDTH-OF-WORLD (* WHEEL-DISTANCE 40))
(define HEIGHT-OF-WORLD (* WHEEL-DISTANCE 20))

;## Graphical constants
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define SPACE
  (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "white"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))
(define CAR-BODY
  (rectangle WIDTH-OF-CAR-BODY HEIGHT-OF-CAR-BODY "solid" "red"))
(define CAR-HEAD
  (rectangle WIDTH-OF-CAR-HEAD HEIGHT-OF-CAR-HEAD "solid" "orange"))
(define CAR-UP
  (overlay/align/offset "middle" "top"
                        CAR-HEAD 0 HEIGHT-OF-CAR-HEAD CAR-BODY))
(define CAR
  (overlay/align/offset "middle" "bottom"
                      BOTH-WHEELS 0 (* -1 WHEEL-RADIUS) CAR-UP)
)
(define HALF-OF-CAR-HEIGHT (/ (image-height CAR) 2))
(define Y-CAR (- HEIGHT-OF-WORLD HALF-OF-CAR-HEIGHT))
(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

(define BACKGROUND (place-image
                    tree
                    (* 2 (/ WIDTH-OF-WORLD 3))
                    (- HEIGHT-OF-WORLD (/ (image-height tree) 2))
                    (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))
)
;## functions
; WorldState -> Image
; places the image of the car x pixels from 
; the left margin of the BACKGROUND image 
; 一半开始太奇怪？
(define (render x)
  (place-image CAR x Y-CAR BACKGROUND)
)

; WorldState -> WorldState
; adds 3 to x to move the car right 
; examples: 
;   given: 20, expect 23
;   given: 78, expect 81
(define (tock x)
  (+ x 3)
)
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

(define (stop x)
  (>= x (+ WIDTH-OF-WORLD (/ (image-width CAR) 2))))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 
(define (hyper x-position-of-car x-mouse y-mouse me)
  (if (string=? me "button-down") x-mouse x-position-of-car)
)



; WorldState, int, int, MouseEvent -> WorldState
; when MouseEvent is "button-down", change ws to x_mouse
; in1: 21 10 20 "enter", out: 21
; in2: 33 5 10 "button-down", out: 5
; in2: 33 5 10 "move", out: 33
(define (hyper2 ws x_mouse y_mouse me)
  (cond
    [(string=? me "button-down") x_mouse]
    [else ws])
)
(check-expect (hyper2 21 10 20 "enter") 21)
(check-expect (hyper2 33 5 10 "button-down") 5)
(check-expect (hyper2 33 5 10 "move") 33)



; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]
     [on-mouse hyper2]
     [stop-when stop]
     )
)
;(main 13)